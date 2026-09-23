use std::env;
use std::fs::{self, OpenOptions};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static TEMP_FILE_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Debug)]
pub struct Options {
    pub output: Option<String>,
    pub compile_only: bool,
    pub assembly_only: bool,
    pub cc1: bool,
    pub print_commands: bool,
    pub cc1_input: Option<String>,
    pub cc1_output: Option<String>,
    pub inputs: Vec<String>,
}

pub fn usage(program: &str) {
    eprintln!("{program} [ -o <path> ] [ -S | -c ] <file>...");
}

pub fn parse_args(args: &[String]) -> Result<Options, String> {
    let mut options = Options {
        output: None,
        compile_only: false,
        assembly_only: false,
        cc1: false,
        print_commands: false,
        cc1_input: None,
        cc1_output: None,
        inputs: Vec::new(),
    };

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--help" => {
                usage(&args[0]);
                std::process::exit(0);
            }
            "-###" => options.print_commands = true,
            "-cc1" => options.cc1 = true,
            "-S" => options.assembly_only = true,
            "-c" => options.compile_only = true,
            "-o" | "-cc1-input" | "-cc1-output" => {
                let option = args[i].clone();
                i += 1;
                let value = args
                    .get(i)
                    .ok_or_else(|| format!("missing argument for '{option}'"))?
                    .clone();

                match option.as_str() {
                    "-o" => options.output = Some(value),
                    "-cc1-input" => options.cc1_input = Some(value),
                    "-cc1-output" => options.cc1_output = Some(value),
                    _ => unreachable!(),
                }
            }
            arg if arg.starts_with("-o") => {
                options.output = Some(arg[2..].to_string());
            }
            "-" => options.inputs.push("-".to_string()),
            arg if arg.starts_with('-') => {
                return Err(format!("unknown argument: {arg}"));
            }
            input => options.inputs.push(input.to_string()),
        }
        i += 1;
    }

    if options.inputs.is_empty() {
        return Err("no input files".to_string());
    }

    if options.inputs.len() > 1
        && options.output.is_some()
        && (options.compile_only || options.assembly_only)
    {
        return Err("cannot specify '-o' with '-c' or '-S' with multiple files".to_string());
    }

    Ok(options)
}

pub fn run(options: &Options, original_args: &[String]) -> Result<(), String> {
    let mut temporary_files = TemporaryFiles::default();
    let mut linker_inputs = Vec::new();

    for input in &options.inputs {
        let output = options.output.clone().unwrap_or_else(|| {
            replace_extension(input, if options.assembly_only { ".s" } else { ".o" })
        });

        if input.ends_with(".o") {
            if !options.compile_only && !options.assembly_only {
                linker_inputs.push(input.clone());
            }
            continue;
        }

        if input.ends_with(".s") {
            if options.assembly_only {
                continue;
            }

            if options.compile_only {
                assemble(input, &output, options.print_commands)?;
            } else {
                let object = temporary_files.create()?;
                assemble(input, path_str(&object)?, options.print_commands)?;
                linker_inputs.push(path_str(&object)?.to_string());
            }
            continue;
        }

        if input != "-" && !input.ends_with(".c") {
            return Err(format!("unknown file extension: {input}"));
        }

        if options.assembly_only {
            run_cc1(original_args, input, &output, options.print_commands)?;
        } else if options.compile_only {
            let assembly = temporary_files.create()?;
            run_cc1(
                original_args,
                input,
                path_str(&assembly)?,
                options.print_commands,
            )?;
            assemble(path_str(&assembly)?, &output, options.print_commands)?;
        } else {
            let assembly = temporary_files.create()?;
            let object = temporary_files.create()?;
            run_cc1(
                original_args,
                input,
                path_str(&assembly)?,
                options.print_commands,
            )?;
            assemble(
                path_str(&assembly)?,
                path_str(&object)?,
                options.print_commands,
            )?;
            linker_inputs.push(path_str(&object)?.to_string());
        }
    }

    if !options.compile_only && !options.assembly_only && !linker_inputs.is_empty() {
        run_linker(
            &linker_inputs,
            options.output.as_deref().unwrap_or("a.out"),
            options.print_commands,
        )?;
    }

    Ok(())
}

fn replace_extension(input: &str, extension: &str) -> String {
    let filename = Path::new(input)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or(input);
    let stem = filename
        .rfind('.')
        .map(|dot| &filename[..dot])
        .unwrap_or(filename);
    format!("{stem}{extension}")
}

fn run_cc1(
    original_args: &[String],
    input: &str,
    output: &str,
    print_command: bool,
) -> Result<(), String> {
    let executable =
        env::current_exe().map_err(|err| format!("cannot find the rust-cc executable: {err}"))?;
    let mut command = Command::new(executable);
    command.args(&original_args[1..]);
    command.args(["-cc1", "-cc1-input", input, "-cc1-output", output]);
    run_subprocess(&mut command, print_command)
}

fn assemble(input: &str, output: &str, print_command: bool) -> Result<(), String> {
    let mut command = Command::new("as");
    command.args(["-c", input, "-o", output]);
    run_subprocess(&mut command, print_command)
}

fn run_linker(inputs: &[String], output: &str, print_command: bool) -> Result<(), String> {
    let library_path = find_library_path()?;
    let gcc_library_path = find_gcc_library_path()?;
    let dynamic_linker = find_dynamic_linker()?;

    let mut command = Command::new("ld");
    command.args(["-o", output, "-m", "elf_x86_64", "-dynamic-linker"]);
    command.arg(dynamic_linker);
    command.arg(library_path.join("crt1.o"));
    command.arg(library_path.join("crti.o"));
    command.arg(gcc_library_path.join("crtbegin.o"));
    command.arg(format!("-L{}", gcc_library_path.display()));
    command.arg(format!("-L{}", library_path.display()));
    command.arg(format!("-L{}", library_path.join("..").display()));

    for path in [
        "/usr/lib64",
        "/lib64",
        "/usr/lib/x86_64-linux-gnu",
        "/usr/lib/x86_64-pc-linux-gnu",
        "/usr/lib/x86_64-redhat-linux",
        "/usr/lib",
        "/lib",
    ] {
        command.arg(format!("-L{path}"));
    }

    command.args(inputs);
    command.args(["-lc", "-lgcc", "--as-needed", "-lgcc_s", "--no-as-needed"]);
    command.arg(gcc_library_path.join("crtend.o"));
    command.arg(library_path.join("crtn.o"));
    run_subprocess(&mut command, print_command)
}

fn run_subprocess(command: &mut Command, print_command: bool) -> Result<(), String> {
    if print_command {
        eprintln!("{}", format_command(command));
    }

    let program = command.get_program().to_string_lossy().into_owned();
    let status = command
        .status()
        .map_err(|err| format!("failed to execute '{program}': {err}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("'{program}' exited with status {status}"))
    }
}

fn format_command(command: &Command) -> String {
    std::iter::once(command.get_program())
        .chain(command.get_args())
        .map(|arg| arg.to_string_lossy())
        .collect::<Vec<_>>()
        .join(" ")
}

fn find_library_path() -> Result<PathBuf, String> {
    ["/usr/lib/x86_64-linux-gnu", "/usr/lib64"]
        .iter()
        .map(PathBuf::from)
        .find(|path| path.join("crti.o").is_file())
        .ok_or_else(|| "C runtime library path was not found".to_string())
}

fn find_gcc_library_path() -> Result<PathBuf, String> {
    let roots = [
        "/usr/lib/gcc/x86_64-linux-gnu",
        "/usr/lib/gcc/x86_64-pc-linux-gnu",
        "/usr/lib/gcc/x86_64-redhat-linux",
    ];
    let mut candidates = Vec::new();

    for root in roots {
        let Ok(entries) = fs::read_dir(root) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.join("crtbegin.o").is_file() {
                candidates.push(path);
            }
        }
    }

    candidates.sort();
    candidates
        .pop()
        .ok_or_else(|| "GCC runtime library path was not found".to_string())
}

fn find_dynamic_linker() -> Result<PathBuf, String> {
    [
        "/lib64/ld-linux-x86-64.so.2",
        "/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2",
    ]
    .iter()
    .map(PathBuf::from)
    .find(|path| path.is_file())
    .ok_or_else(|| "dynamic linker was not found".to_string())
}

fn path_str(path: &Path) -> Result<&str, String> {
    path.to_str()
        .ok_or_else(|| format!("path is not valid UTF-8: {}", path.display()))
}

#[derive(Default)]
struct TemporaryFiles {
    paths: Vec<PathBuf>,
}

impl TemporaryFiles {
    fn create(&mut self) -> Result<PathBuf, String> {
        for _ in 0..1000 {
            let id = TEMP_FILE_ID.fetch_add(1, Ordering::Relaxed);
            let path = env::temp_dir().join(format!("rust-cc-{}-{id}", std::process::id()));
            match OpenOptions::new().write(true).create_new(true).open(&path) {
                Ok(_) => {
                    self.paths.push(path.clone());
                    return Ok(path);
                }
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(err) => {
                    return Err(format!(
                        "cannot create temporary file {}: {err}",
                        path.display()
                    ));
                }
            }
        }
        Err("cannot create a unique temporary file".to_string())
    }
}

impl Drop for TemporaryFiles {
    fn drop(&mut self) {
        for path in &self.paths {
            let _ = fs::remove_file(path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::replace_extension;

    #[test]
    fn output_is_created_in_the_current_directory() {
        assert_eq!(replace_extension("dir/example.c", ".o"), "example.o");
        assert_eq!(replace_extension("example", ".s"), "example.s");
        assert_eq!(replace_extension("-", ".o"), "-.o");
    }
}
