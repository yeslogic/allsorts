# Allsorts C API

A C API for the Allsorts font shaping engine.

# Header generation

We automatically generate C headers using the [cbindgen](https://github.com/eqrion/cbindgen).
In lieu of better options for distributing these headers, the following environment variables
can be configured to alter cbindgen's behaviour:

- `ALLSORTS_CBINDGEN_OUT_DIR`: The output directory in which to generate `allsorts.h`
- `ALLSORTS_CBINDGEN_CONFIG_FILE`: The filename of the TOML file used to configure cbindgen

Note that these variables are _relative to the `allsorts-capi` source directory_, which is
usually not what you want. You will have to supply a full path in order to generate it
anywhere else.

## License

The Allsorts C API is distributed under the terms of the Apache License (Version 2.0).

See [LICENSE](LICENSE) for details.
