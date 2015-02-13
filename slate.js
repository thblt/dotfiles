// Configs

S.cfga({
	"defaultToCurrentScreen" : true,
	"secondsBetweenRepeat" : 0.1,
	"checkDefaultsOnLoad" : true,
	"focusCheckWidthMax" : 3000,
	"orderScreensLeftToRight" : true,
	"modalEscapeKey": "f19"
});

// Monitors

var monMain = "2560x1440";

// Operations

var mov_TL_2_2 = S.op("move", {
	"x" : "screenOriginX",
	"y" : "screenOriginY",
	"width" : "screenSizeX/2",
	"height" : "screenSizeY/2"
});

var mov_TR_2_2 = S.op("move", {
	"x" : "screenOriginX+(screenSizeX/2)",
	"y" : "screenOriginY",
	"width" : "screenSizeX/2",
	"height" : "screenSizeY/2"
});


var mov_BL_2_2 = S.op("move", {
	"x" : "screenOriginX",
	"y" : "screenOriginY",
	"width" : "screenSizeX/2",
	"height" : "screenSizeY/2"
});

var mov_BR_2_2 = S.op("move", {
	"x" : "screenOriginX+(screenSizeX/2)",
	"y" : "screenOriginY",
	"width" : "screenSizeX/2",
	"height" : "screenSizeY/2"
});


S.bnda({
	// "pad7" : mov_TL_2_2,
	// "pad9" : mov_TR_2_2,
	
    "esc:ctrl" : S.op("grid", { grids : { "2560x1440" : { "width" : 12, "height": 12 }}} ),
	
    "f19:ctrl" : S.op("relaunch")
	
});
