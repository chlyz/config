use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::os::unix::fs::symlink;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use serde::Deserialize;

const PACKAGES: [&str; 2] = ["files", "scripts"];

#[derive(Debug)]
struct Link {
    source: PathBuf,
    destination: PathBuf,
}

#[derive(Deserialize)]
struct SystemManifest {
    system: System,
    packages: toml::Table,
    #[serde(default)]
    uv_tools: BTreeMap<String, Vec<String>>,
}

#[derive(Deserialize)]
struct System {
    distribution: String,
}

enum LinkState {
    Linked,
    Missing,
    Conflict,
}

fn usage() {
    println!(
        "Usage: config <command> [--dry-run]\n\nCommands:\n  link                    Link this repository into $HOME\n  unlink                  Remove links owned by this repository\n  status                  Show the state of managed links\n  update                  Update stable Rust and repository tools\n  packages install        Install system packages and user tools\n  packages upgrade        Upgrade system packages and user tools\n  packages <command> --dry-run\n                          Print package and tool commands without running them\n  help                    Show this help"
    );
}

fn run(program: &str, arguments: &[&str]) -> Result<(), ()> {
    let status = match Command::new(program).args(arguments).status() {
        Ok(status) => status,
        Err(error) => {
            eprintln!("config: could not run {program}: {error}");
            return Err(());
        }
    };

    if status.success() { Ok(()) } else { Err(()) }
}

fn update() -> ExitCode {
    let bootstrap = Path::new(env!("CARGO_MANIFEST_DIR")).join("bootstrap");
    let bootstrap = bootstrap.to_string_lossy();
    if run("bash", &[&bootstrap]).is_ok() {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn current_distribution() -> Result<String, String> {
    let os_release = fs::read_to_string("/etc/os-release")
        .map_err(|error| format!("could not read /etc/os-release: {error}"))?;
    let distribution = os_release
        .lines()
        .find_map(|line| line.strip_prefix("ID="))
        .ok_or_else(|| "could not find ID in /etc/os-release".to_owned())?;
    Ok(distribution.trim_matches('"').to_owned())
}

fn package_manager(distribution: &str) -> Option<(&str, &[&str])> {
    match distribution {
        "fedora" => Some(("dnf", &["install", "--refresh", "--assumeyes"])),
        "ubuntu" => Some(("apt-get", &["install", "--yes"])),
        _ => None,
    }
}

fn package_names(manifest: &SystemManifest) -> Result<BTreeSet<String>, String> {
    let mut names = BTreeSet::new();
    for (group, packages) in &manifest.packages {
        let packages = packages
            .as_array()
            .ok_or_else(|| format!("packages.{group} must be an array"))?;
        for package in packages {
            let package = package
                .as_str()
                .ok_or_else(|| format!("packages.{group} must contain only strings"))?;
            names.insert(package.to_owned());
        }
    }
    Ok(names)
}

fn run_uv_tools(
    tools: &BTreeMap<String, Vec<String>>,
    install: bool,
    dry_run: bool,
) -> Result<(), ()> {
    for (tool, dependencies) in tools {
        let mut arguments = vec!["tool".to_owned()];
        if install {
            arguments.push("install".to_owned());
            for dependency in dependencies {
                arguments.push("--with".to_owned());
                arguments.push(dependency.clone());
            }
        } else {
            arguments.push("upgrade".to_owned());
        }
        arguments.push(tool.clone());

        if dry_run {
            println!("uv {}", arguments.join(" "));
        } else {
            let arguments: Vec<&str> = arguments.iter().map(String::as_str).collect();
            run("uv", &arguments)?;
        }
    }
    Ok(())
}

fn install_packages(dry_run: bool) -> ExitCode {
    let distribution = match current_distribution() {
        Ok(distribution) => distribution,
        Err(error) => {
            eprintln!("config: {error}");
            return ExitCode::FAILURE;
        }
    };
    let Some((manager, manager_arguments)) = package_manager(&distribution) else {
        eprintln!("config: unsupported distribution: {distribution}");
        return ExitCode::FAILURE;
    };

    let manifest_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("systems")
        .join(format!("{distribution}.toml"));
    let manifest = match fs::read_to_string(&manifest_path)
        .map_err(|error| format!("could not read {}: {error}", manifest_path.display()))
        .and_then(|contents| {
            toml::from_str::<SystemManifest>(&contents)
                .map_err(|error| format!("could not parse {}: {error}", manifest_path.display()))
        }) {
        Ok(manifest) => manifest,
        Err(error) => {
            eprintln!("config: {error}");
            return ExitCode::FAILURE;
        }
    };
    if manifest.system.distribution != distribution {
        eprintln!(
            "config: {} declares {}, not {distribution}",
            manifest_path.display(),
            manifest.system.distribution
        );
        return ExitCode::FAILURE;
    }
    let packages = match package_names(&manifest) {
        Ok(packages) => packages,
        Err(error) => {
            eprintln!("config: {error}");
            return ExitCode::FAILURE;
        }
    };

    let mut arguments: Vec<&str> = manager_arguments.to_vec();
    arguments.extend(packages.iter().map(String::as_str));
    if dry_run {
        println!("sudo {manager} {}", arguments.join(" "));
    } else if run(
        "sudo",
        &[manager]
            .iter()
            .chain(arguments.iter())
            .copied()
            .collect::<Vec<_>>(),
    )
    .is_err()
    {
        return ExitCode::FAILURE;
    }

    match run_uv_tools(&manifest.uv_tools, true, dry_run) {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn upgrade_packages(dry_run: bool) -> ExitCode {
    let distribution = match current_distribution() {
        Ok(distribution) => distribution,
        Err(error) => {
            eprintln!("config: {error}");
            return ExitCode::FAILURE;
        }
    };
    let commands: &[(&str, &[&str])] = match distribution.as_str() {
        "fedora" => &[("dnf", &["upgrade", "--refresh", "--assumeyes"])],
        "ubuntu" => &[("apt-get", &["update"]), ("apt-get", &["upgrade", "--yes"])],
        _ => {
            eprintln!("config: unsupported distribution: {distribution}");
            return ExitCode::FAILURE;
        }
    };

    for (program, arguments) in commands {
        if dry_run {
            println!("sudo {program} {}", arguments.join(" "));
        } else if run(
            "sudo",
            &[*program]
                .iter()
                .chain(arguments.iter())
                .copied()
                .collect::<Vec<_>>(),
        )
        .is_err()
        {
            return ExitCode::FAILURE;
        }
    }
    let manifest_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("systems")
        .join(format!("{distribution}.toml"));
    let manifest = match fs::read_to_string(&manifest_path)
        .map_err(|error| format!("could not read {}: {error}", manifest_path.display()))
        .and_then(|contents| {
            toml::from_str::<SystemManifest>(&contents)
                .map_err(|error| format!("could not parse {}: {error}", manifest_path.display()))
        }) {
        Ok(manifest) => manifest,
        Err(error) => {
            eprintln!("config: {error}");
            return ExitCode::FAILURE;
        }
    };

    match run_uv_tools(&manifest.uv_tools, false, dry_run) {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn mapped_component(component: &OsStr) -> PathBuf {
    let component = component.to_string_lossy();
    match component.strip_prefix("dot-") {
        Some(rest) => PathBuf::from(format!(".{rest}")),
        None => PathBuf::from(component.as_ref()),
    }
}

fn mapped_path(relative_path: &Path) -> PathBuf {
    relative_path
        .components()
        .map(|component| mapped_component(component.as_os_str()))
        .collect()
}

fn collect_files(directory: &Path, files: &mut Vec<PathBuf>) -> io::Result<()> {
    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        let path = entry.path();
        let metadata = entry.file_type()?;

        if metadata.is_dir() {
            collect_files(&path, files)?;
        } else if metadata.is_file() {
            files.push(path);
        } else {
            eprintln!(
                "config: skipping unsupported source entry {}",
                path.display()
            );
        }
    }
    Ok(())
}

fn managed_links(repository: &Path, target: &Path) -> io::Result<Vec<Link>> {
    let mut links = Vec::new();

    for package in PACKAGES {
        let package_path = repository.join(package);
        let mut files = Vec::new();
        collect_files(&package_path, &mut files)?;

        for source in files {
            let relative_path = source
                .strip_prefix(&package_path)
                .expect("source is in package");
            let destination = target.join(mapped_path(relative_path));
            links.push(Link {
                source,
                destination,
            });
        }
    }

    links.sort_by(|left, right| left.destination.cmp(&right.destination));
    Ok(links)
}

fn link_state(link: &Link) -> io::Result<LinkState> {
    match fs::symlink_metadata(&link.destination) {
        Ok(metadata) if metadata.file_type().is_symlink() => {
            if fs::canonicalize(&link.destination)? == fs::canonicalize(&link.source)? {
                Ok(LinkState::Linked)
            } else {
                Ok(LinkState::Conflict)
            }
        }
        Ok(_) => Ok(LinkState::Conflict),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(LinkState::Missing),
        Err(error) => Err(error),
    }
}

fn target_directory() -> Result<PathBuf, String> {
    env::var_os("CONFIG_TARGET")
        .or_else(|| env::var_os("HOME"))
        .map(PathBuf::from)
        .ok_or_else(|| "CONFIG_TARGET or HOME must be set".to_owned())
}

fn link(dry_run: bool) -> ExitCode {
    let repository = Path::new(env!("CARGO_MANIFEST_DIR"));
    let target = match target_directory() {
        Ok(target) => target,
        Err(error) => {
            eprintln!("config: {error}");
            return ExitCode::FAILURE;
        }
    };
    let links = match managed_links(repository, &target) {
        Ok(links) => links,
        Err(error) => {
            eprintln!("config: could not read managed files: {error}");
            return ExitCode::FAILURE;
        }
    };

    let mut conflicts = Vec::new();
    for link in &links {
        match link_state(link) {
            Ok(LinkState::Conflict) => conflicts.push(link),
            Ok(_) => {}
            Err(error) => {
                eprintln!(
                    "config: could not inspect {}: {error}",
                    link.destination.display()
                );
                return ExitCode::FAILURE;
            }
        }
    }

    if !conflicts.is_empty() {
        for conflict in conflicts {
            eprintln!(
                "config: refusing to overwrite {}",
                conflict.destination.display()
            );
        }
        return ExitCode::FAILURE;
    }

    for link in links {
        if matches!(link_state(&link), Ok(LinkState::Linked)) {
            continue;
        }

        if dry_run {
            println!(
                "link {} -> {}",
                link.destination.display(),
                link.source.display()
            );
            continue;
        }

        if let Some(parent) = link.destination.parent()
            && let Err(error) = fs::create_dir_all(parent)
        {
            eprintln!("config: could not create {}: {error}", parent.display());
            return ExitCode::FAILURE;
        }
        if let Err(error) = symlink(&link.source, &link.destination) {
            eprintln!(
                "config: could not link {}: {error}",
                link.destination.display()
            );
            return ExitCode::FAILURE;
        }
        println!("linked {}", link.destination.display());
    }

    ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maps_dot_prefixes_in_every_path_component() {
        assert_eq!(
            mapped_path(Path::new("dot-config/emacs/dot-init.el")),
            PathBuf::from(".config/emacs/.init.el")
        );
    }

    #[test]
    fn recognises_only_links_to_the_managed_source() {
        let temporary_directory = env::temp_dir().join(format!(
            "config-cli-test-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("system clock is after the Unix epoch")
                .as_nanos()
        ));
        let source = temporary_directory.join("source");
        let destination = temporary_directory.join("destination");
        let other_source = temporary_directory.join("other-source");

        fs::create_dir_all(&temporary_directory).expect("create temporary directory");
        fs::write(&source, "source").expect("write source");
        fs::write(&other_source, "other").expect("write other source");
        symlink(&source, &destination).expect("create managed link");
        let link = Link {
            source: source.clone(),
            destination: destination.clone(),
        };
        assert!(matches!(link_state(&link), Ok(LinkState::Linked)));

        fs::remove_file(&destination).expect("remove managed link");
        symlink(&other_source, &destination).expect("create foreign link");
        assert!(matches!(link_state(&link), Ok(LinkState::Conflict)));

        fs::remove_dir_all(&temporary_directory).expect("remove temporary directory");
    }

    #[test]
    fn collects_and_deduplicates_manifest_packages() {
        let manifest: SystemManifest = toml::from_str(
            r#"
                [system]
                distribution = "fedora"

                [packages]
                editors = ["neovim", "emacs"]
                shell = ["fzf", "neovim"]

                [uv_tools]
                mdformat = ["mdformat-gfm"]
            "#,
        )
        .expect("parse manifest");

        assert_eq!(
            package_names(&manifest).expect("collect packages"),
            BTreeSet::from(["emacs".to_owned(), "fzf".to_owned(), "neovim".to_owned()])
        );
        assert_eq!(
            manifest.uv_tools,
            BTreeMap::from([("mdformat".to_owned(), vec!["mdformat-gfm".to_owned()])])
        );
    }
}

fn unlink(dry_run: bool) -> ExitCode {
    let repository = Path::new(env!("CARGO_MANIFEST_DIR"));
    let target = match target_directory() {
        Ok(target) => target,
        Err(error) => {
            eprintln!("config: {error}");
            return ExitCode::FAILURE;
        }
    };
    let links = match managed_links(repository, &target) {
        Ok(links) => links,
        Err(error) => {
            eprintln!("config: could not read managed files: {error}");
            return ExitCode::FAILURE;
        }
    };

    for link in links {
        if !matches!(link_state(&link), Ok(LinkState::Linked)) {
            continue;
        }
        if dry_run {
            println!("unlink {}", link.destination.display());
            continue;
        }
        if let Err(error) = fs::remove_file(&link.destination) {
            eprintln!(
                "config: could not unlink {}: {error}",
                link.destination.display()
            );
            return ExitCode::FAILURE;
        }
        println!("unlinked {}", link.destination.display());
    }

    ExitCode::SUCCESS
}

fn status() -> ExitCode {
    let repository = Path::new(env!("CARGO_MANIFEST_DIR"));
    let target = match target_directory() {
        Ok(target) => target,
        Err(error) => {
            eprintln!("config: {error}");
            return ExitCode::FAILURE;
        }
    };
    let links = match managed_links(repository, &target) {
        Ok(links) => links,
        Err(error) => {
            eprintln!("config: could not read managed files: {error}");
            return ExitCode::FAILURE;
        }
    };

    let mut failed = false;
    for link in links {
        let label = match link_state(&link) {
            Ok(LinkState::Linked) => "linked",
            Ok(LinkState::Missing) => "missing",
            Ok(LinkState::Conflict) => {
                failed = true;
                "conflict"
            }
            Err(error) => {
                failed = true;
                eprintln!(
                    "config: could not inspect {}: {error}",
                    link.destination.display()
                );
                "error"
            }
        };
        println!("{label:8} {}", link.destination.display());
    }

    if failed {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn command_with_dry_run(arguments: &[String], action: fn(bool) -> ExitCode) -> ExitCode {
    match arguments {
        [] => action(false),
        [flag] if flag == "--dry-run" => action(true),
        _ => {
            eprintln!("config: expected only --dry-run");
            ExitCode::FAILURE
        }
    }
}

fn packages(arguments: &[String]) -> ExitCode {
    match arguments {
        [command] if command == "install" => install_packages(false),
        [command, flag] if command == "install" && flag == "--dry-run" => install_packages(true),
        [command] if command == "upgrade" => upgrade_packages(false),
        [command, flag] if command == "upgrade" && flag == "--dry-run" => upgrade_packages(true),
        _ => {
            eprintln!("config: expected packages install|upgrade [--dry-run]");
            ExitCode::FAILURE
        }
    }
}

fn main() -> ExitCode {
    let arguments: Vec<String> = env::args().skip(1).collect();
    match arguments.first().map(String::as_str) {
        Some("link") => command_with_dry_run(&arguments[1..], link),
        Some("unlink") => command_with_dry_run(&arguments[1..], unlink),
        Some("status") if arguments.len() == 1 => status(),
        Some("update") if arguments.len() == 1 => update(),
        Some("packages") => packages(&arguments[1..]),
        Some("help" | "--help" | "-h") | None => {
            usage();
            ExitCode::SUCCESS
        }
        _ => {
            eprintln!("config: unknown command or arguments");
            usage();
            ExitCode::FAILURE
        }
    }
}
