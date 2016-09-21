#!/usr/bin/python

import getpass
import re
import subprocess


def which_security():
    return subprocess.check_output(["which", "security"]).strip()


def get_keychain_pass(account=None, server=None):
    user = getpass.getuser()
    security = which_security()
    security_cmd = 'find-generic-password'
    keychain = '/Users/%s/Library/Keychains/login.keychain' % user

    command = "sudo -u %s %s -v %s -g -a %s -s %s %s" % (
        user, security, security_cmd, account, server, keychain)
    output = subprocess.check_output(
        command, shell=True, stderr=subprocess.STDOUT)
    password_line = [line for line in output.splitlines()
                     if line.startswith('password: ')][0]
    return re.match(r'password: "(.*)"', password_line).group(1)
