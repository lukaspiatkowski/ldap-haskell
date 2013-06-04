#!/bin/bash

ldapmodify -h localhost -p 5002 -D "" -f barbaraMod.ldif
