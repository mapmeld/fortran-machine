! oscommands.dos --
!     OS commands UNIX-style
!
!     To be included in the OS utils module for UNIX/Linux/OS X systems
!
character(len=10), parameter          :: system_type   = "unix"
character(len=1),  parameter          :: dir_separator = "/"

character(len=20), parameter, private :: copy          = "cp"
character(len=20), parameter, private :: rename        = "mv"
character(len=20), parameter, private :: remove        = "rm -f"
character(len=20), parameter, private :: bg_cmd_prefix = "nohup"
character(len=20), parameter, private :: bg_cmd_suffix = "&"
character(len=20), parameter, private :: filelist      = "ls"     ! Only file names!
character(len=20), parameter, private :: mkdir         = "mkdir"
character(len=20), parameter, private :: rmdir         = "rmdir"
character(len=20), parameter, private :: redirect      = ">"
character(len=20), parameter, private :: suppress_msg  = "2>/dev/null"
character(len=20), parameter, private :: tempdir       = "/tmp/"
logical,           parameter, private :: add_dir       = .false.

