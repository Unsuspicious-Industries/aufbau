use std::fs;
use std::path::Path;

const ENGINE_DIR: &str = "src/engine";
const FORBIDDEN_PATH: &str = "domains::typing";

fn main() {
    let engine = Path::new(ENGINE_DIR);
    if !engine.exists() {
        return;
    }

    let mut errors: Vec<String> = Vec::new();
    collect_rs_files(engine, &mut errors);

    if !errors.is_empty() {
        for err in &errors {
            println!("cargo:warning={}", err);
        }
        panic!(
            "engine module must not import from domains::typing\n{}",
            errors.join("\n")
        );
    }
}

fn collect_rs_files(dir: &Path, errors: &mut Vec<String>) {
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().is_some_and(|n| n == "tests") {
                continue;
            }
            collect_rs_files(&path, errors);
        } else if path.extension().is_some_and(|e| e == "rs") {
            if path.file_name().is_some_and(|n| n == "tests.rs") {
                continue;
            }
            check_file(&path, errors);
        }
    }
}

fn check_file(path: &Path, errors: &mut Vec<String>) {
    let content = fs::read_to_string(path).unwrap();
    let rel = path.to_string_lossy().replace('\\', "/");
    let lines: Vec<&str> = content.lines().collect();

    let mut in_cfg_test_depth: Option<i32> = None;
    let mut pending_cfg_test = false;
    let mut depth: i32 = 0;

    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();

        if trimmed.contains("#[cfg(test)]") {
            pending_cfg_test = true;
        }

        for ch in trimmed.chars() {
            match ch {
                '{' => {
                    depth += 1;
                    if pending_cfg_test {
                        in_cfg_test_depth = Some(depth);
                        pending_cfg_test = false;
                    }
                }
                '}' => {
                    if Some(depth) == in_cfg_test_depth {
                        in_cfg_test_depth = None;
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }

        if in_cfg_test_depth.is_none() && line.contains(FORBIDDEN_PATH) {
            errors.push(format!("  {}:{}: {}", rel, i + 1, line.trim()));
        }
    }
}
