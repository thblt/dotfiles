#!/usr/bin/env python
# -*- coding: utf-8 -*-

#                                                                 [licblock]
# This file is part of Tkacz. 
# Copyright (c) 2012-2014 Thibault Polge. All rights reserved.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#                                                                [/licblock]

""" 
@file
@brief A script for inserting/updating license blocks in source files.
""" 

import sys, os, re

# begin and end markers for license blocks. Used when updating.
marks = ("[licblock]", "[/licblock]")

""" 
@brief Comment templates py language types.
@param start Comment block start delimiter
@param inter Intermediate comment lines prefix
@param end Comment block end delimiter 
(nb: for line comments, use start=None, inter="//", last=None, …), see <code>templates["python"]</code>.
@param respect a re object (or any object who defines <code>bool match(self, str)</code> to identify  
       lines which must be kept at the top of the file. (for shebangs and the like)
"""
templates = dict();
templates["c"] = {"start":"/* ", "inter":" * ", "end":" */", "respect":None}
templates["python"] = {"start":None, "inter":"# ", "end":None, "respect":re.compile("(^#!)|(^#.*-\*-)")}
templates["xml"] = {"start":"<!--", "inter":"", "end":"-->", "respect":re.compile("(^<\?xml)|(^\<\!DOCTYPE)")}

# Mapping extensions to templates.
extensions = {
              "c": "c",
              "h":"c",
              "h.in":"c",
              "cpp":"c",
              "cxx":"c",
			  "gengetopt": "python",
              "hpp":"c",
              "py":"python",
              "htm":"xml",
              "html":"xml",
              "xhtml":"xml",
              "xml":"xml",
              "ui":"xml",
              }

ws = re.compile("\s+")

def usage(cmdName):
    print("Usage:\n\t{0} license file […file]\n".format(cmdName))
    print("Options:")
    print("\tlicense\tThe license text to insert.")
    print("\tfile\tThe files to process. If the given file is a directory,\n\t\tit will be search recursively.")

def readFile(path):
    ret = []
    with open(path, 'r') as input:
        line = input.readline()
        while (line):
            ret.append(line)
            line = input.readline()
    return ret

def do(f, licText):
    if os.path.isdir(f):
        # Recurse
        for g in [os.path.join(f, x) for x in os.listdir(f)]:
            do(g, licText)
    else:
        # Find template for file by extension.
        # We don't use os.path.splitext because it doesn't allow to easily 
        # identify double extensions such as "h.in" or "xml.tpl".
        tpl = None
        for ext in extensions.keys():
            # and (len(os… : Ignore dotfiles.
            if f.endswith("." + ext) and (len(os.path.split(f)[1]) > len(ext) + 1):
                tplName = extensions[ext]
                tpl = templates[tplName]
                break
        if not tpl:
            # print("Skipping\t{0}\t(unrecognized type)".format(f))
            return False
    
        contents = readFile(f)
        
        maxLen = max(len(line.strip()) for line in licText)
        
        tplStart = tpl["start"] if tpl["start"] else tpl["inter"]
        tplEnd = tpl["end"] if tpl["end"] else ""
        
        startMark = tplStart + (maxLen - 1 - len(marks[0]) + len(tpl["inter"]) - len(tplStart)) * " " + marks[0]
        endMark = tpl["inter"] + (maxLen - 1 - len(marks[1])) * " " + marks[1] + tplEnd 
        
        start = None  # 
        end = None
        respect = -1
        respectByMatching = False  # Avoids leaving useless blank lines on top of file.

        strippedStartMark = ws.subn(" ", startMark)[0]
        strippedEndMark = ws.subn(" ", endMark)[0]

        # Scanning input

        for i in range(0, len(contents)):
            line = contents[i]
            strippedLine = ws.subn(" ", line)[0][:-1]  # [:-1] strips \n
            if strippedLine == strippedStartMark:
                if start:
                    print("Aborting:\t{0}\tStart mark found twice, lines {1} and {2}).".format(f, start, i))
                    return
                start = i
            elif strippedLine == strippedEndMark:
                end = i
                break;
            elif not line.strip() and respect == i - 1:
                respect = i 
            if respect == i - 1 and tpl["respect"] and tpl["respect"].match(line):
                respect = i
                respectByMatching = True 
                
        respect = respect if (respectByMatching and start == None) or (start == None) else -2;        
          
        if ((start == None) != (end == None)) or ((start != None) and end <= start):
            print("Aborting:\t{0}\tParse error ({1},{2},{3})".format(f, start, end, respect))
            return
          
        if start != None:
            print("Replacing in:\t{0}\t({1} file)".format(f, extensions[ext]))
        else:
            print("Adding in:\t{0}\t({1} file)".format(f, extensions[ext]))
            
        output = open(f, 'w')
        
        for i in range(0, len(contents)):
            if i == respect + 1 or i == start:
                if respect >= 0: 
                    output.write("\n")  # Blank line before only on first insertion.
                output.write(startMark + "\n")
                for licLine in licText:
                    output.write("{0}{1}".format(tpl["inter"], licLine))
                output.write("\n" + endMark + "\n\n")
            if start == None or (i > end + 1) or (i < start):
                output.write(contents[i])

if __name__ == "__main__":
    if len(sys.argv) < 3:
        usage(sys.argv[0])
        exit(-1)
        
    licText = readFile(sys.argv[1]) 
        
    for f in sys.argv[2:]:
        do(f, licText)
