use std::fs;
use std::path::Path;

fn main() {
    // Skip the line-count check when building inside a manylinux container
    // (CI) or when the script isn't available.
    if std::env::var("MATURIN_PYPI_TOKEN").is_ok()
        || std::env::var("PYO3_PYTHON").is_ok()
        || std::env::var("CI").is_ok()
    {
        return;
    }
    let lc_script = Path::new("scripts/lc.sh");
    if !lc_script.exists() {
        return;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(lc_script).unwrap().permissions();
        perms.set_mode(perms.mode() | 0o111);
        fs::set_permissions(lc_script, perms).unwrap();
    }
    let lc_status = std::process::Command::new("scripts/lc.sh")
        .arg("--check")
        .status()
        .unwrap_or_else(|e| panic!("failed to run scripts/lc.sh: {e}"));
    assert!(
        lc_status.success(),
        "line-count budgets exceeded — run ./scripts/lc.sh for details"
    );
}
