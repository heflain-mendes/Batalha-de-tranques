class Colors{
    constructor(firstColor=0) {
        this.colors=["LightBlue", "LightGreen", "LightYellow", "LightGrey", "LightPink", "LightSalmon", "LightSeaGreen",
                    "LightCoral", "LightCyan", "Indigo  ", "Gold", "MediumSpringGreen",
                    "Linen", "Peru", "LemonChiffon", "SaddleBrown", "HoneyDew",
                    "Tan", "Brown", "PaleTurquoise", "Silver", "DimGrey", "OldLace", "PapayaWhip", "Cornsilk",
                    "Bisque", "OliveDrab", "Purple", "HotPink", "PaleGoldenRod",
                    "SandyBrown", "Orchid", "CadetBlue", "RosyBrown", "AntiqueWhite", "ForestGreen", "Khaki",
                    "Crimson", "PeachPuff", "Olive", "Ivory", "GhostWhite", "Maroon", "GreenYellow", "GoldenRod",
                    "Magenta", "Violet", "Navy", "MediumAquaMarine", "SteelBlue", "Moccasin", "AliceBlue",
                    "LimeGreen", "Aqua", "Turquoise", "OrangeRed", "Beige", "Teal", "Fuchsia", "SpringGreen", "MintCream",
                    "Salmon", "DeepSkyBlue", "WhiteSmoke", "MediumSlateBlue", "Green", "DimGray", "LawnGreen", "Gray",
                    "DodgerBlue", "PowderBlue", "Grey", "SlateGray", "BlanchedAlmond", "LavenderBlush", "MediumBlue",
                    "Tomato", "RebeccaPurple", "Chocolate", "MistyRose", "Orange", "DeepPink", "MediumPurple", "Plum",
                    "BurlyWood", "MediumSeaGreen", "YellowGreen", "CornflowerBlue", "Lime", "MediumTurquoise", "Thistle",
                    "Pink", "FloralWhite", "Snow", "Gainsboro", "FireBrick", "MediumOrchid", "Yellow", "NavajoWhite",
                    "PaleGreen", "Cyan", "Lavender", "IndianRed ", "Sienna", "SlateGrey", "Wheat", "Chartreuse", "Aquamarine",
                    "Blue", "Azure", "Coral", "SeaGreen"];
        this.colorPosition = firstColor%this.colors.length;
    }
    getColor() {
        let pos = this.colorPosition;
        this.colorPosition = (this.colorPosition+1)%this.colors.length;
        return this.colors[pos];
    }
}