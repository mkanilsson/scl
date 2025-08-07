use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Clone, Debug)]
pub enum Cli {
    /// Build an scl project or file
    #[command()]
    Build { file: PathBuf },

    /// Build an scl project or file and run resulting executable
    #[command()]
    Run { file: PathBuf },

    /// Show scl env configuration
    #[command()]
    Env,
}
