#! /usr/bin/env python
from subprocess import check_output
import ast, os, re

def get_pass(account):
    return check_output("pass Mail/" + account, shell=True).splitlines()[0]

def parse_netrc(lines):
    # Python parser is broken on many accounts
    re_start = re.compile("^machine (.*)$")
    re_field = re.compile("^\s+([a-z]+)\s+(.*)$")

    ret = dict()
    curdata = dict()
    curname = None
    lno = 0 # Line counter
    
    for line in lines:
        lno += 1
        machine = re_start.match(line)
        if machine:
            curname = machine.groups()[0]
            ret[curname] = dict()
        else:
            field = re_field.match(line)
            if not curname:
                raise Exception("Field outside a machine at line {0}".format(lno))

            if field:
                value = field.groups()[1].encode().decode('unicode_escape')
                # Is the value between quotes?
                if value[0] == '"' and value[-1] == '"':
                    value = value[1:-1]
                ret[curname][field.groups()[0]] = value 

    return ret
    

def get_netrc_pass(hostname):
    with open(os.path.expanduser("~/.netrc"), "r") as f_netrc:
        netrc = parse_netrc(f_netrc.readlines())
        
        return netrc[hostname]["password"]
