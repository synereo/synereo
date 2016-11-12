#! /usr/bin/env bash

set -e

source_dir=${1:-"$(pwd)/../ProductWebUI"}
publish_dir="$(pwd)/gloseval/client"
js_asset_dir="sclient/target/scala-2.11/"
js_assets=( client-jsdeps.min.js client-launcher.js client-opt.js )
js_assets=( "${js_assets[@]/#/$js_asset_dir}" )
other_assets=( server/target/web/public/main/* )

echo
echo =================
echo Source directory:
echo =================

cd $source_dir

echo $(pwd)

echo
echo ======================
echo Destination directory:
echo ======================

echo $publish_dir

echo
echo =============================
echo Generating assets with SBT...
echo =============================

sbt -v ";clean ;assets ;fullOptJS"

echo
echo ==============================================================
echo Creating destination directory if it does not already exist...
echo ==============================================================

rm -r $publish_dir/*
mkdir -p $publish_dir/assets

echo
echo ====================
echo Copying JS assets...
echo ====================

for a in ${js_assets[@]}; do
    echo Copying ${a}
    cp ${a} $publish_dir/assets
done

echo
echo =======================
echo Copying other assets...
echo =======================

for a in ${other_assets[@]}; do
    echo Copying ${a}
    cp -r ${a} $publish_dir/assets
done

echo
echo ============================
echo Manipulating other assets...
echo ============================

find $publish_dir/assets/index.s.html -type f -exec mv {} $publish_dir/index.html \;
