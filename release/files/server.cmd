@setlocal

@rem Get the absolute path to the parent directory,
@rem which is assumed to be the node root.
@for /F "delims=" %%I in ("%~dp0..") do @set node_root=%%~fI

@cd %node_root%

@set releases_dir=%node_root%\releases

@rem Parse ERTS version and release version from start_erl.data
@for /F "usebackq tokens=1,2" %%I in ("%releases_dir%\start_erl.data") do @(
    @call :set_trim erts_version %%I
    @call :set_trim release_version %%J
)

@call etc\config.cmd

@set node_boot_script=%releases_dir%\%release_version%\%RELEASE%
@set clean_boot_script=%releases_dir%\%release_version%\start_clean

@IF NOT EXIST start_clean.boot copy /Y %clean_boot_script%.boot start_clean.boot
@IF NOT EXIST start_clean.script copy /Y %clean_boot_script%.script start_clean.script

@rem extract erlang cookie from vm.args
@for /f "usebackq tokens=1-2" %%I in (`findstr /b \-setcookie "%VM_ARGS%"`) do @set erlang_cookie=%%J
@for /f "usebackq tokens=1-2" %%I in (`findstr /b \-name "%VM_ARGS%"`) do @set node_name=%%J

@set erts_bin=%node_root%\erts-%erts_version%\bin

@set erlsrv="%erts_bin%\erlsrv.exe"
@set epmd="%erts_bin%\epmd.exe"
@set escript="%erts_bin%\escript.exe"
@set werl="%erts_bin%\werl.exe"
@set erl="%erts_bin%\erl.exe"
@set nodetool="%erts_bin%\nodetool"

@if "%1"=="usage" @goto usage
@if "%1"=="console" @goto console
@if "%1"=="ping" @goto ping
@if "%1"=="rpc" @goto rpc
@echo Unknown command: "%1"

:usage
@echo Usage: %~n0 [console^|ping^|rpc]
@goto :EOF

:console
@start "%node_name% console" %werl% -boot "%node_boot_script%" -config "%APP_CONFIG%" -args_file "%VM_ARGS%"
@goto :EOF

:ping
@%escript% %nodetool% ping -name %node_name% -setcookie "%erlang_cookie%"
@goto :EOF

:rpc
@%escript% %nodetool% -name %node_name% -setcookie "%erlang_cookie%" rpc %2 %3 %4 %5 %6 %7
@goto :EOF

:set_trim
@set %1=%2
@goto :EOF
