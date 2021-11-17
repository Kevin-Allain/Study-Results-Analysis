console.log("---- FunctionProlificControl.js")
// https://paper.dropbox.com/doc/Notes-about-coding-for-results-analysis--BIcFyz4C8sfOP9sNxjJhTANNAQ-2QtCwBvX4Vng9cWFJjlds
var $ = require("jquery");
const csvparser = require('csv-parser');
const fs = require('fs');
// I'm going to transform the csv into json, modify it accordingly, and then create a new csv.
const csv = require('csvtojson')

var csvFilePath = 'data/complete_20210828_noflip_headerAdapted.csv'
let rawdata=fs.readFileSync('data/QIDtoFilename_complete_20210828_noflip.json')
let QIDtoFilename = JSON.parse(rawdata);
hashmapAttributesNames_glbl = {
    "eG": "engine temperature",
    "eT": "engine temperature", // there's a bug in the data generation, but not a big issue
    "fC": "fuel consumption",
    "eC": "electricity consumption",
    "sF": "suspension force",
    "gO": "gps on",
    "wO": "wiper on",
    "rO": "radio on",
    "hO": "heating on"
}

function extractColumnsFromFilename(filename, hashmapAttributesNames = hashmapAttributesNames_glbl) {
    var idc = Number(filename.substr("idc".length, filename.indexOf('-') - "idc".length));
    var splitFN = filename.split('-');
    var drawnQn = hashmapAttributesNames[splitFN[1]];
    var drawnQl = hashmapAttributesNames[splitFN[2]];
    var queryArr = splitFN[3].split('_');
    console.log("for queryString... hashmapAttributesNames: ",hashmapAttributesNames+", queryArr[1]: ",queryArr[1],", queryArr[2]: ",queryArr[2],", queryArr[3]: ",queryArr[3],", splitFN: ",splitFN)
    var queryString = hashmapAttributesNames[queryArr[1]] + " " + queryArr[2] + " " + queryArr[3]
    var flips = splitFN[4].substr(splitFN[4].indexOf('_') + 1)
    var nMasks = Number(splitFN[5].substr(splitFN[5].indexOf('_') + 1))
    var dMask = splitFN[6].substr(splitFN[6].indexOf('_') + 1)
    var dComplex = splitFN[7].substr(splitFN[7].indexOf('_') + 1); 
    var dComplex_Qn = dComplex[0], dComplex_Ql = dComplex[1], dComplex_Where = dComplex[2];
    var focus = splitFN[8].substr(splitFN[8].indexOf('_') + 1);
    var bslnTot = splitFN[9].split('_');
    var bslnA = null;
    var bslnA1 = null, bslnA2 = null, bslnA3 = null;
    if (bslnTot[1].indexOf(')') !== "undefined") {
        var strSplitBlsn = bslnTot[1].split(')')
        // bslnA1 = Number(bslnTot[1].substr(0, bslnTot[1].indexOf(')')));
        // bslnA2 = Number(bslnTot[1].substr(bslnTot[1].indexOf(')') + 1, 5));
        // bslnA3 = Number(bslnTot[1].substr(bslnTot[1].length - 5));
        bslnA1 = Number(strSplitBlsn[0]);
        bslnA2 = Number(strSplitBlsn[1]);
        bslnA3 = Number(strSplitBlsn[2]);
    } else {
        bslnA = (splitFN[9].substr(splitFN[9].indexOf('_') + 1, 1) === 't') ? 1 : -1;
    }
    var bslnB = (splitFN[9].substr(splitFN[9].length - 1) === 't') ? 1 : -1;
    var cntrQ = Number(splitFN[10].substr(splitFN[10].indexOf('_') + 1));

    if (bslnA !== null) {
        return {
            "idc": idc, "drawnQn": drawnQn, "drawnQl": drawnQl, "queryString": queryString, "flips": flips, "nMasks": nMasks, "dMask": dMask, "dComplex_Qn": dComplex_Qn, "dComplex_Ql": dComplex_Ql, "dComplex_Where": dComplex_Where, "focus": focus,
            "bslnA": bslnA, "bslnB": bslnB, "cntrQ": cntrQ
        }
    } else {
        return {
            "idc": idc, "drawnQn": drawnQn, "drawnQl": drawnQl, "queryString": queryString, "flips": flips, "nMasks": nMasks, "dMask": dMask, "dComplex_Qn": dComplex_Qn, "dComplex_Ql": dComplex_Ql, "dComplex_Where": dComplex_Where, "focus": focus,
            "bslnA1": bslnA1, "bslnA2": bslnA2, "bslnA3": bslnA3, "bslnB": bslnB, "cntrQ": cntrQ
        }
    }
}

function filterWrongAnswersToIntro(csvFilePath){
    var baselinesIntro = {
        "Q11":"Down",
        "Q12": "From time 10 to 30 and time 60 to 70",
        "Q13":"Time 30",
        "Q17":"On the first long straight section",
        "Q14":"From a little before time 70 up to a little before time 80"
    }
    var objGenerated = [];
    var toFilterProlificIds = [];
    csv()
        .fromFile(csvFilePath)
        .then((objJson) => {
            for (var k in objJson) {
                if (k >= 0) {
                    for (var key in objJson[k]) {
                        if (objJson[k][key] !== '') {
                            if (key == "Q11" || key == "Q12"  || key == "13" || key == "17" || key == "14")
                            {
                                if (objJson[k][key] != baselinesIntro[key])
                                {
                                    // Get the prolific id...
                                    toFilterProlificIds.push(objJson[k]["Q15"])
                                }
                            }
                        }
                    }
                }
            }       
        })
    return toFilterProlificIds;
}