# Package

version       = "0.1.0"
author        = "Jason"
description   = "A new awesome nimble package"
license       = "MIT"
srcDir        = "src"
bin           = @["raymarch"]


# Dependencies

requires "nim >= 1.4.6"
requires "vmath"
requires "sdl2_nim"
requires "opengl"