#!/usr/bin/env -S godot --headless --script
extends SceneTree

func _init():
	print("hello godot!")
	print("args", OS.get_cmdline_args())
	quit()
