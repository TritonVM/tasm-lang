//! Runs `rustc` in-process on an in-memory crate and hands the type context
//! to a callback, which is expected to extract whatever it needs (for this
//! compiler: THIR bodies) before the compiler is torn down again.

use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use rustc_driver::Callbacks;
use rustc_driver::Compilation;
use rustc_interface::interface::Compiler;
use rustc_interface::interface::Config;
use rustc_middle::ty::TyCtxt;

/// The compiler front-end is not re-entrant: `rustc` keeps global state
/// (e.g. the interner for spans) that must not be shared between concurrent
/// compilations. So compilations are serialized.
static COMPILATION_LOCK: Mutex<()> = Mutex::new(());

/// The crate name given to the compiled program. Used to recognize paths that
/// point into the program (as opposed to `core`/`std`).
pub(crate) const CRATE_NAME: &str = "tasm_lang_program";

/// Extracts whatever is needed from the type context of the compiled program.
type Extractor<'f, T> = Box<dyn FnOnce(TyCtxt<'_>) -> T + Send + 'f>;

struct ExtractionCallbacks<'f, T> {
    extractor: Option<Extractor<'f, T>>,
    result: Option<T>,
}

impl<T> Callbacks for ExtractionCallbacks<'_, T> {
    fn config(&mut self, _config: &mut Config) {}

    fn after_expansion<'tcx>(&mut self, _compiler: &Compiler, tcx: TyCtxt<'tcx>) -> Compilation {
        // Run name resolution, type collection and well-formedness checks for
        // all items, and type-check all bodies. Any error found here makes
        // `rustc` print diagnostics; the caller is informed through
        // `has_errors` below.
        rustc_hir_analysis::check_crate(tcx);
        for def_id in tcx.hir_body_owners() {
            tcx.ensure_ok().typeck(def_id);
        }

        if tcx.dcx().has_errors().is_none() {
            let extractor = self.extractor.take().unwrap();
            let result = extractor(tcx);

            // Borrow checking consumes the THIR, so it can only run after the
            // extraction. It reports, e.g., assignments to immutable variables.
            for def_id in tcx.hir_body_owners() {
                tcx.ensure_ok().mir_borrowck(def_id);
            }
            if tcx.dcx().has_errors().is_none() {
                self.result = Some(result);
            }
        }

        // Nothing more is needed from `rustc`. In particular, no code
        // generation must happen.
        Compilation::Stop
    }
}

/// Compile `source` as a library crate and pass the resulting type context to
/// `extractor`. Returns `None` if the program does not compile; in that case
/// `rustc` has already printed the diagnostics to stderr.
pub(crate) fn with_type_context<T: Send>(
    source: String,
    extractor: impl FnOnce(TyCtxt<'_>) -> T + Send,
) -> Option<T> {
    let _guard = COMPILATION_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());

    // `rustc` wants a file. (It could read from stdin, but that happens before
    // the callbacks get a chance to substitute an in-memory input.)
    let source_file = SourceFile::write(&source);

    let args = [
        "rustc",
        source_file.path.to_str().unwrap(),
        "--crate-type=lib",
        &format!("--crate-name={CRATE_NAME}"),
        "--edition=2021",
        "--cap-lints=allow",
        "-Zno-codegen",
    ]
    .map(str::to_owned);

    let mut callbacks = ExtractionCallbacks {
        extractor: Some(Box::new(extractor)),
        result: None,
    };

    // Fatal errors in `rustc` are implemented as panics with a special
    // payload; catch those instead of letting them unwind through the caller.
    let _ = rustc_driver::catch_fatal_errors(|| rustc_driver::run_compiler(&args, &mut callbacks));
    drop(source_file);

    callbacks.result
}

/// A temporary file holding the source of the program. Removed on drop.
struct SourceFile {
    path: PathBuf,
}

impl SourceFile {
    fn write(source: &str) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let unique_id = format!(
            "{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::Relaxed)
        );
        let path = std::env::temp_dir().join(format!("tasm-lang-program-{unique_id}.rs"));
        std::fs::write(&path, source).expect("must be able to write program to temporary file");
        Self { path }
    }
}

impl Drop for SourceFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}
