#!/usr/bin/env python3

from PIL import Image, ImageDraw
import sys

width = 14
height = 14
borderWidth = 0
strokeWidth = 1
strokeColor = 0
bgColor = 255

def beginImage(**elements):
	img = Image.new("1", (width, height))
	draw = ImageDraw.Draw(img)

	drawImage(draw, **elements)

	return (img, draw)

def endImage(img, name):
	img.save("layout_{0}.xbm".format(name))
	sys.stdout.write(" {0} : <icon=./layout_{0}.xbm/>".format(name))
	
	del img

def drawImage(draw, v2=False, v2_s2=False, v2_e2=False, h2=False, h2_s2=False, h2_e2=False):

	# 
	# Fill image
	draw.rectangle( (0, 0, width, height), fill=strokeColor)
	# Clean center
	draw.rectangle( (borderWidth, borderWidth, width-borderWidth-1, height-borderWidth-1), fill=bgColor)
	# Little dots for rounding corner
	#draw.point( ( (0,0), (0,height-1), (width-1,0), (width-1,height-1) ), fill=fillColor if borderWidth else strokeColor )

	if v2 or v2_s2: # Start half of a vertical half-cut
		draw.rectangle( (width/2-strokeWidth/2, 0, width/2+strokeWidth/2-1, height/2-1), fill=strokeColor )

	if v2 or v2_e2: # End half of same
		draw.rectangle( (width/2-strokeWidth/2, height/2, width/2+strokeWidth/2-1, height), fill=strokeColor )

	if h2 or h2_s2: # Start half on an horizontal half-cut
		draw.rectangle( (0, height/2-strokeWidth/2, width/2-1, height/2+strokeWidth/2-1), fill=strokeColor )
		
	if h2 or h2_e2: # End half of same.
		draw.rectangle( (width/2, height/2-strokeWidth/2, width, height/2+strokeWidth/2-1), fill=strokeColor )
		
def makeImage(name, **elements):
	img, draw = beginImage(**elements)
	del draw
	endImage(img, name)

makeImage("Full")
makeImage("Tall", v2=True, h2_e2=True)
makeImage("Mirror Tall", h2=True, v2_e2=True)
makeImage("Grid", h2=True, v2=True)

# Three columns
img, draw = beginImage(v2=True)
draw.rectangle( (width//(4/3)-strokeWidth/2+1, 0, width//(4/3)+strokeWidth/2, height), fill=strokeColor )
del draw
endImage(img, "ThreeCol")

# Spiral
img, draw = beginImage(v2=True)

draw.rectangle( (width/2, height//(7/4)-strokeWidth/2, width, height//(7/4)+strokeWidth/2-1), fill=strokeColor )
draw.rectangle( (width//(4/3)-1, height//(7/4)-strokeWidth/2, width//(4/3)+strokeWidth/2-1, height), fill=strokeColor )
del draw
endImage(img, "Spiral")
