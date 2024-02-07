use std::{
    fs::{create_dir_all, read_to_string, write},
    path::PathBuf,
};

pub fn file_to_string(file_name: &PathBuf) -> Result<String, std::io::Error> {
    read_to_string(file_name)
}

pub fn save_string_to_file(output: &str, file_name: &PathBuf) -> Result<(), std::io::Error> {
    file_name
        .parent()
        .map(create_dir_all)
        .unwrap_or(Ok(()))
        .and_then(|_| write(file_name, output))
}
