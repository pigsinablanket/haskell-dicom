# haskell-dicom

A Haskell library for parsing DICOM medical imaging files.

## Usage

Use `readDicomFile` if the contents are stored on disk.

When the contents of the DICOM file are in memory, pass the bytestring to `parseDicomFile`.

## Building

This project is built with Nix Flakes. To build from source, run `nix build`
