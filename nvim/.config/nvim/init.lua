-- Enable Aniseed's automatic compilation and loading of Fennel source code.
-- TODO safe compile (don't panic on compilation errors)
require("aniseed.env").init({module = "core", compile = true})

-- TODO handle this chicken and egg
require("impatient")

-- ensure packages and their configuration
local packages = require("packages")
packages.setup_packages()

-- require fennel core, which should be compiled above by aniseed#env
print("requiring core")
local core = require("core")
core.setup()
