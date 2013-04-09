@echo off
if "%1"=="" (
	setlocal enabledelayedexpansion
	set /p plugins="Plugin(s):"
	java -server -Xms3g -Xmx5g -XX:+PrintCommandLineFlags -jar ./target/platformagentlauncher-1.0.0.0-jar-with-dependencies.jar ui !plugins!
	endlocal
) else (
	java -server -Xms3g -Xmx5g -XX:+PrintCommandLineFlags -jar ./target/platformagentlauncher-1.0.0.0-jar-with-dependencies.jar ui %*
)
