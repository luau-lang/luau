#!usr/bin/python3
"""
To use this command, simply run the command:
`command script import /path/to/your/game-engine/Client/Luau/tools/stackdbg.py`
in the `lldb` interpreter. You can also add it to your .lldbinit file to have it be
automatically imported.

If using vscode, you can add the above command to your launch.json under `preRunCommands` for the appropriate target. For example:
{
    "name": "Luau.UnitTest",
    "type": "lldb",
    "request": "launch",
    "program": "${workspaceFolder}/build/ninja/common-tests/noopt/Luau/Luau.UnitTest",
    "preRunCommands": [
        "command script import ${workspaceFolder}/Client/Luau/tools/stackdbg.py"
    ],
}

Once this is loaded,
`(lldb) help stack`
or
`(lldb) stack -h
or
`(lldb) stack --help

can get you started
"""

import lldb
import functools
import argparse
import shlex

# Dumps the collected frame data
def dump(collected):
    for (frame_name, size_in_kb, live_size_kb, variables) in collected:
        print(f'{frame_name}, locals: {size_in_kb}kb, fp-sp:  {live_size_kb}kb')
        for (var_name, var_size, variable_obj) in variables:
            print(f'    {var_name}, {var_size} bytes')

def dbg_stack_pressure(frame, frames_to_show = 5, sort_frames = False, vars_to_show = 5, sort_vars = True):
    totalKb = 0
    collect = []
    for f in frame.thread:
        frame_name = f.GetFunctionName()
        variables = [ (v.GetName(), v.GetByteSize(), v) for v in f.get_locals() ]
        if sort_vars:
            variables.sort(key = lambda x: x[1], reverse = True)
        size_in_kb = functools.reduce(lambda x,y : x + y[1], variables, 0) / 1024

        fp = f.GetFP()
        sp = f.GetSP()
        live_size_kb = round((fp - sp) / 1024, 2)

        size_in_kb = round(size_in_kb, 2)
        totalKb += size_in_kb
        collect.append((frame_name, size_in_kb, live_size_kb, variables[:vars_to_show]))
    if sort_frames:
        collect.sort(key = lambda x: x[1], reverse = True)

    print("******************** Report Stack Usage ********************")
    totalMb = round(totalKb / 1024, 2)
    print(f'{len(frame.thread)} stack frames used {totalMb}MB')
    dump(collect[:frames_to_show])

def stack(debugger, command, result, internal_dict):
    """
    usage: [-h] [-f FRAMES] [-fd] [-v VARS] [-vd]

    optional arguments:
    -h, --help            show this help message and exit
    -f FRAMES, --frames FRAMES
                        How many stack frames to display
    -fd, --sort_frames    Sort frames
    -v VARS, --vars VARS  How many variables per frame to display
    -vd, --sort_vars      Sort frames
    """

    frame = debugger.GetSelectedTarget().GetProcess().GetSelectedThread().GetSelectedFrame()
    args = shlex.split(command)
    argparser = argparse.ArgumentParser(allow_abbrev = True)
    argparser.add_argument("-f", "--frames", required=False, help="How many stack frames to display", default=5, type=int)
    argparser.add_argument("-fd", "--sort_frames", required=False, help="Sort frames in descending order of stack usage", action="store_true", default=False)
    argparser.add_argument("-v", "--vars", required=False, help="How many variables per frame to display", default=5, type=int)
    argparser.add_argument("-vd", "--sort_vars", required=False, help="Sort locals in descending order of stack usage ", action="store_true", default=False)

    args = argparser.parse_args(args)
    dbg_stack_pressure(frame, frames_to_show=args.frames, sort_frames=args.sort_frames, vars_to_show=args.vars, sort_vars=args.sort_vars)

# Initialization code to add commands
def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f stackdbg.stack stack')
    print("The 'stack' python command has been installed and is ready for use.")

