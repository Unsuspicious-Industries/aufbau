use std::fs;
use std::path::Path;

fn main() {
    // ── Line-count budget check (STYLE.md §1) ────────────────────────────
    let lc_script = Path::new("scripts/lc.sh");
    if lc_script.exists() {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(lc_script).unwrap().permissions();
            perms.set_mode(perms.mode() | 0o111);
            fs::set_permissions(lc_script, perms).unwrap();
        }
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
