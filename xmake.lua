set_xmakever('2.9.2')
add_rules('mode.release', 'mode.debug')
set_policy('build.ccache', not is_plat('windows'))
includes('xmake/xmake_func.lua')

target('luau_base')
set_kind('phony')
add_defines('LUA_USE_LONGJMP', {public = true})
add_includedirs('Common/include', {public = true})

target('luau_vm')
_config_project({
    project_kind = 'static'
})
add_files('VM/src/*.cpp')
add_includedirs('VM/include', 'VM/src', {
    public = true
})
add_deps('luau_base')
target_end()


target('luau_codegen')
_config_project({
    project_kind = 'static'
})
add_files('Codegen/src/*.cpp')
add_includedirs('Codegen/include', {
    public = true
})
add_deps('luau_vm')
target_end()


target('luau_ast')
_config_project({
    project_kind = 'static',
    enable_exception = true
})
add_files('Ast/src/*.cpp')
add_includedirs('Ast/include', {
    public = true
})
add_deps('luau_base')
target_end()

target('luau_compiler')
_config_project({
    project_kind = 'static',
    enable_exception = true
})
add_files('Compiler/src/*.cpp')
add_includedirs('Compiler/include', {
    public = true
})
add_deps('luau_ast')
target_end()


target('luau_config')
_config_project({
    project_kind = 'static',
})
add_files('Config/src/*.cpp')
add_includedirs('Config/include', {
    public = true
})
add_deps('luau_ast')
target_end()


target('luau_analysis')
_config_project({
    project_kind = 'static',
    enable_exception = true
})
add_files('Analysis/src/*.cpp')
add_includedirs('Analysis/include', {
    public = true
})
add_deps('luau_config')
target_end()


target('isocline')
_config_project({project_kind = 'static',})
add_includedirs('extern/isocline/include', {public = true})
add_files('extern/isocline/src/isocline.c')
target_end()


target('luau')
_config_project({
    project_kind = 'binary',
    enable_exception = true
})
add_files('CLI/ReplEntry.cpp', 'CLI/Repl.cpp', 'CLI/Flags.cpp', 'CLI/FileUtils.cpp', 'CLI/Profiler.cpp', 'CLI/Coverage.cpp', 'CLI/Require.cpp')
add_deps('luau_compiler', 'luau_analysis', 'luau_vm', 'luau_codegen', 'isocline')
target_end()

