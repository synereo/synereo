#! /usr/bin/env bash

socat TCP4-LISTEN:${1:-2376},fork,reuseaddr UNIX-CONNECT:/var/run/docker.sock
