package main

import (
	"github.com/bitfield/script"
	"github.com/jessevdk/go-flags"
	"os"
)

var opts struct {
	Redis    bool   `short:"r" long:"redis" description:"Whether or not to run Redis"`
	TestPath string `short:"t" long:"path" description:"Path to test file"`
}

var redisProc *script.Pipe

func main() {
	_, err := flags.Parse(&opts)

	if err != nil {
		panic(err)
	}

	if opts.Redis {
		script.Echo("Running Redis server\n").Stdout()
		redisProc = script.Exec("redis-server")
	}

	script.Echo("Running tests\n").Stdout()
	mixProc := script.Exec("mix test " + opts.TestPath + " --color")
	mixProc.Stdout()

	exitCode := mixProc.ExitStatus()

	if opts.Redis {
		script.Echo("Trying to close Redis server\n").Stdout()
		closeRedis()
	}

	os.Exit(exitCode)
}

func closeRedis() {
	// Find the pid and... kill it
	pid, err := script.Exec("pgrep 6379").String()

	if err != nil {
		panic(err)
	}

	script.Exec("kill -s TERM " + pid).Stdout()
}
