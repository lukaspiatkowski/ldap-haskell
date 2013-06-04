#!/bin/bash

ldapsearch -h localhost -p 5002 -D "" "cn=Barbara Jensen" -b "dc=com"
