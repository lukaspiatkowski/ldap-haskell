#!/bin/bash

ldapadd -h localhost -p 5002 -D "" -f barbara.ldif
