use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::os::unix::fs::symlink;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

const PACKAGES: [&str; 2] = ["files", "scripts"];

#[derive(Debug)]
struct Link {
    source: PathBuf,
    destination: PathBuf,
}

enum LinkState {
    Linked,
    Missing,
    Conflict,
}

fn usage() {
    println!(
        "Usage: config <command> [--dry-run]\n\nCommands:\n  link      Link this repository into $HOME\n  unlink    Remove links owned by this repository\n  status    Show the state of managed links\n  update    Update stable Rust and repository tools\n  help      Show this help"
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

fn main() -> ExitCode {
    let arguments: Vec<String> = env::args().skip(1).collect();
    match arguments.first().map(String::as_str) {
        Some("link") => command_with_dry_run(&arguments[1..], link),
        Some("unlink") => command_with_dry_run(&arguments[1..], unlink),
        Some("status") if arguments.len() == 1 => status(),
        Some("update") if arguments.len() == 1 => update(),
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
