use std::env;
use std::process::{Command, ExitCode};

fn usage() {
    println!(
        "Usage: config <command>\n\nCommands:\n  update    Update stable Rust and repository tools\n  help      Show this help"
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
    if run("rustup", &["update", "stable"]).is_err() {
        eprintln!("Run ./bootstrap first on a clean system.");
        return ExitCode::FAILURE;
    }

    if run(
        "rustup",
        &[
            "run",
            "stable",
            "cargo",
            "install",
            "rumdl",
            "--version",
            "0.2.52",
            "--locked",
        ],
    )
    .is_err()
    {
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

fn main() -> ExitCode {
    match env::args().nth(1).as_deref() {
        Some("update") => update(),
        Some("help" | "--help" | "-h") | None => {
            usage();
            ExitCode::SUCCESS
        }
        Some(command) => {
            eprintln!("config: unknown command {command:?}");
            usage();
            ExitCode::FAILURE
        }
    }
}
