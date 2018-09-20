# Kazoo Core Application: `kazoo_oauth`

## About

This a standard Erlang OTP application. Please adhere to OTP directory structure below (do not put any documentation in this file or directory!):

```
    ─ ${application}
      ├── doc
      │   ├── internal
      │   ├── api
      │   └── guide
      ├── include
      ├── priv
      ├── src
      │   └── ${application}.app.src
      └── test
```

* `src`: Contains the Erlang source code, the source of the `.app` file and internal include files used by the application itself. Additional sub-directories within `src` can be used as name spaces to organize source files. These directories should never be deeper than one level.
* `priv`: Used for application specific files.
* `include`: Used for public include files that must be reachable from other applications.
* `doc`: Any documentation should be placed in sub-directories here. 2600Hz documentation mandates this directory structure:
    * `doc/internal`: Any documentation that describes implementation details about this application, not intended for 2600Hz official publications, should be placed here.
    * `doc/api`: Any documentation that describes usage of this application for users developing using Kazoo HTTP API or integration developers should be placed here.
    * `doc/guide`: All documentations intended for users, administrators and resellers or reference manual or user guides should be placed here.
* `test`: All files regarding tests, such as unit test and test specifications, should be placed here.
