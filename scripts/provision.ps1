# Convenience script for setting up a development environment on Windows

Add-Type -AssemblyName System.IO.Compression.FileSystem

#Erlang

$erlangExePath = "$($env:USERPROFILE)\esl-erlang_15.b.3-1~windows_amd64.exe"

if (!(Test-Path -Path $erlangExePath)) {
    Write-Host "Downloading Erlang..." -ForegroundColor Cyan
    (New-Object Net.WebClient).DownloadFile('https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_15.b.3-1~windows_amd64.exe', $erlangExePath)
}

Write-Host "Installing Erlang..."
cmd /c start /wait $erlangExePath /S

Write-Host "Erlang installed" -ForegroundColor Green

# RabbitMQ
# https://github.com/appveyor/ci/issues/307

$rabbitExePath = "$($env:USERPROFILE)\rabbitmq-server-2.7.1.exe"

if (!(Test-Path -Path $rabbitExePath)) {
    Write-Host "Downloading RabbitMQ..." -ForegroundColor Cyan
    (New-Object Net.WebClient).DownloadFile('http://www.rabbitmq.com/releases/rabbitmq-server/v2.7.1/rabbitmq-server-2.7.1.exe', $rabbitExePath)
}

Write-Host "Installing RabbitMQ..."
cmd /c start /wait $rabbitExePath /S

$rabbitPath = 'C:\Program Files (x86)\RabbitMQ Server\rabbitmq_server-2.7.1'

Write-Host "Installing service..."
Start-Process -Wait "$rabbitPath\sbin\rabbitmq-service.bat" "install"

Write-Host "Starting service..."
Start-Process -Wait "$rabbitPath\sbin\rabbitmq-service.bat" "start"

Get-Service "RabbitMQ"

Write-Host "RabbitMQ installed and started" -ForegroundColor Green

# MongoDB
# https://github.com/appveyor/ci/issues/328

$mongoZipPath = "$($env:USERPROFILE)\mongodb-win32-x86_64-2008plus-2.6.12.zip"

if (!(Test-Path -Path $mongoZipPath)) {
    Write-Host "Downloading MongoDB..." -ForegroundColor C
    (New-object Net.WebClient).DownloadFile('http://downloads.mongodb.org/win32/mongodb-win32-x86_64-2008plus-2.6.12.zip', $mongoZipPath)
}

Write-Host "Installing MongoDB..."

[System.IO.Compression.ZipFile]::ExtractToDirectory($mongoZipPath, "C:\")

mv 'c:\mongodb-win32-x86_64-2008plus-2.6.12' c:\mongodb

mkdir c:\mongodb\data\db | Out-Null
mkdir c:\mongodb\log | Out-Null

'systemLog:
    destination: file
    path: c:\mongodb\log\mongod.log
storage:
    dbPath: c:\mongodb\data\db' | Out-File C:\mongodb\mongod.cfg -Encoding utf8

cmd /c start /wait sc.exe create MongoDB binPath= "C:\mongodb\bin\mongod.exe --service --config=C:\mongodb\mongod.cfg" DisplayName= "MongoDB" start= "demand"

& c:\mongodb\bin\mongod --version

Start-Service MongoDB

Write-Host "MongoDB installed" -ForegroundColor Green

# SBT

$sbtZipPath = "$($env:USERPROFILE)\sbt-0.13.12.zip"

if (!(Test-Path -Path $sbtZipPath)) {
    Write-Host "Downloading sbt.." -ForegroundColor Cyan
    (new-object System.Net.WebClient).DownloadFile('https://dl.bintray.com/sbt/native-packages/sbt/0.13.12/sbt-0.13.12.zip', $sbtZipPath)
}

Write-Host "Installing sbt..."
[System.IO.Compression.ZipFile]::ExtractToDirectory($sbtZipPath, "C:\")
[System.Environment]::SetEnvironmentVariable("PATH", $env:Path + ";C:\sbt\bin", "User")

Write-Host "sbt installed" -ForegroundColor Green
