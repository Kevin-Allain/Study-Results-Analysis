console.log("---- dataTransform.js")
// https://paper.dropbox.com/doc/Notes-about-coding-for-results-analysis--BIcFyz4C8sfOP9sNxjJhTANNAQ-2QtCwBvX4Vng9cWFJjlds
var $ = require("jquery");
const csvparser = require('csv-parser');
const fs = require('fs');
// I'm going to transform the csv into json, modify it accordingly, and then create a new csv.
const csv = require('csvtojson')
console.log(Math.random())
// ---- Data sources & global variables
// const csvFilePath='data/survey_precise-study_20210410_1500_April 10, 2021_07.36.csv'
// const csvFilePath='data/survey_precise-study_20210410_1500_April 10, 2021_07.36_modified.csv'
// slider_noflip_rightParenthesisBsln_headerReduced
// var csvFilePath='data/slider_noflip_rightParenthesisBsln_headerReduced.csv'
// var csvFilePath='data/FullStudy_final_precise_noflip_automatedAnswers.csv'
// var csvFilePath='data/test_reordered_question_20210721_1547.csv';
// var csvFilePath="data/js_update_questions_reordereed_headFiltered_20210722_1136.csv"
var csvFilePath = "data/new_generation_processing_headFiltered_20210730_1220.csv"

// $.getJSON("QIDtoFilename.json", function(json) {
// let rawdata = fs.readFileSync('data/QIDtoFilename.json');
// let rawdata =  fs.readFileSync('data/QIDtoFilename_test_20210505_1607.json');
// let rawdata =  fs.readFileSync('data/QIDtoFilename_test_20210523_1023.json');
// let rawdata =  fs.readFileSync('data/QIDtoFilename_test_20210525_1618.json');
// let rawdata =  fs.readFileSync('data/QIDtoFilename_20210528_1555.json');
// let rawdata = fs.readFileSync('data/QIDtoFilename_20210722_1136.json');
let rawdata = fs.readFileSync('data/QIDtoFilename_20210802_1914.json');

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

// ---- Function definitions

// e.g. idc39-eG-wO-qry_fC_gte_98.05-flips_h0v0-nMasks_2-dMask_easy-dCmplx_EHE-fcs_WHAT_Qn-bsln_t_t-cntrQ_251
function extractColumnsFromFilename(filename, hashmapAttributesNames = hashmapAttributesNames_glbl) {
    var idc = Number(filename.substr("idc".length, filename.indexOf('-') - "idc".length));
    var splitFN = filename.split('-');
    var drawnQn = hashmapAttributesNames[splitFN[1]];
    var drawnQl = hashmapAttributesNames[splitFN[2]];
    var queryArr = splitFN[3].split('_');
    var queryString = hashmapAttributesNames[queryArr[1]] + " " + queryArr[2] + " " + queryArr[3]
    var flips = splitFN[4].substr(splitFN[4].indexOf('_') + 1)
    var nMasks = Number(splitFN[5].substr(splitFN[5].indexOf('_') + 1))
    var dMask = splitFN[6].substr(splitFN[6].indexOf('_') + 1)
    var dComplex = splitFN[7].substr(splitFN[7].indexOf('_') + 1); var dComplex_Qn = dComplex[0], dComplex_Ql = dComplex[1], dComplex_Where = dComplex[2];
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


function newGenerateModifiedCSV(QIDtoFilename, csvFilePath, addRandomInfoToFillVoid = false) {
    // console.log("newGenerateModifiedCSV ",{QIDtoFilename,csvFilePath,addRandomInfoToFillVoid})
    // console.log(QIDtoFilename);
    var objGenerated = [];
    var cptForBundlingQuestions = 0;
    var storeResponseId = "";
    var storeStartDate = "", storeEndDate = "", storeProgress = "", storeDuration_in_seconds = "", storeFinished = "", storeRecordedDate = "";

    var baseIndx_frstClck = 0;

    csv()
        .fromFile(csvFilePath)
        .then((objJson) => {

            // indx for values which are shared for all the participants. mod is for answers over several questions
            var indxStartDate = 1, indxEndDate = 2, indxStatus = 3, indxProgress = 5, indxDuration_seconds = 6, indxFinished = 7, indxRecordedDate = 8, indxResponseID = 9;
            var fileName = null, idc = null, drawnQn = null, drawnQl = null, queryString = null, flips = null, nMasks = null, dMask = null, dComplex_Qn = null, dComplex_Ql = null, dComplex_Where = null,
                focus = null, bslnA1 = null, bslnA2 = null, bslnA3 = null, bslnB = null, cntrQ = null;
            var modAnswerTime = null, modAnswerA1 = null, modAnswerA2 = null, modAnswerA3 = null, modAnswerB = null, modTrustA1 = null, modTrustA2 = null, modTrustA3 = null, modTrustB = null

            // console.log("QIDtoFilename: ", QIDtoFilename);
            // console.log({objJson});


            for (var k in objJson) {
                console.log("k: ", k,", objJson[k]: ",objJson[k]);
                if (k >= 0) {
                    // Keep a boolean to indicate if the QID has a page submit...
                    var begAnswerIndx = null;
                    var firstAnswerGenerated = false;
                    var objShared = {};
                    console.log("objJson[k][0]: ",objJson[k][0],', objJson[k]["StartDate"]: ', objJson[k]["StartDate"])
                    for (var key in objJson[k]) {
                        // console.log("objJson[k]: ",objJson[k],", key: ",key);
                        // console.log("k: ",k,", key: ",key);
                        if (objJson[k][key] !== '') {
                            // console.log("objJson[k][key] !== ''. objJson[k][key]: ",objJson[k][key])
                            if (key[0] !== "Q") {
                                // console.log('key[0]!=="Q"', "__key: ", key, "(typeof key): ", (typeof key), ", objJson[k][key]: ", objJson[k][key], ", (typeof objJson[k][key]): ", (typeof objJson[k][key]));
                                // objGenerated[objGenerated.length-1][key]=objJson[k][key]
                                objShared[key] = objJson[k][key]
                            } else {
                                // var strAttr=null;
                                // (curIndx===1)?strAttr="StartDate":(curIndx===2)?strAttr="EndDate":(curIndx===3)?strAttr="Status":(curIndx===5)?strAttr="Progress":(curIndx===6)?strAttr="Duration_in_seconds" :(curIndx===7)?strAttr="Finished":(curIndx===8)?strAttr="RecordedDate":(curIndx===9)?strAttr="ResponseId":strAttr="";
                                // if(strAttr!==null)objGenerated[objGenerated.length-1][strAttr]=objJson[k][key];

                                var curIndx = (key.indexOf('_') !== -1) ? Number(key.substr(1, key.indexOf('_') - 1)) : Number(key.substr(1));
                                console.log("key: ", key, "(typeof key): ", (typeof key), ", objJson[k][key]: ", objJson[k][key], ", (typeof objJson[k][key]): ", (typeof objJson[k][key]), ", curIndx: ", curIndx)

                                if (curIndx > 23) {
                                    var keyQual = "QID" + curIndx;
                                    var filename = QIDtoFilename[keyQual]
                                    if (key.indexOf("First") !== -1) {
                                        var objInfo = extractColumnsFromFilename(filename);
                                        objGenerated.push({}); console.log("found the first part. ", { objInfo }, { objShared })
                                        for (var sharedK in objShared) {
                                            if (sharedK.indexOf("Duration") !== -1) {
                                                objGenerated[objGenerated.length - 1]["Duration_in_seconds"] = objShared[sharedK]
                                            } else {
                                                objGenerated[objGenerated.length - 1][sharedK] = objShared[sharedK]
                                            }
                                        }
                                        for (var infoKey in objInfo) {
                                            objGenerated[objGenerated.length - 1][infoKey] = objInfo[infoKey]
                                        }
                                        console.log("Filled after first, ", objGenerated[objGenerated.length - 1])
                                    }
                                    if (key.indexOf("Submit") !== -1) {
                                        begAnswerIndx = curIndx;
                                        console.log("Found Submit, objInfo: ", objInfo);
                                        objGenerated[objGenerated.length - 1]['t'] = objJson[k][key];
                                    } else {
                                        console.log("begAnswerIndx: ",begAnswerIndx)
                                        if (curIndx > begAnswerIndx) {
                                            console.log("objGenerated[objGenerated.length - 1]: ",objGenerated[objGenerated.length - 1]);
                                            (curIndx - begAnswerIndx === 1) ? objGenerated[objGenerated.length - 1]["answerA1"] = objJson[k][key] :
                                            (curIndx - begAnswerIndx === 2) ? objGenerated[objGenerated.length - 1]["answerA2"] = objJson[k][key] :
                                            (curIndx - begAnswerIndx === 3) ? objGenerated[objGenerated.length - 1]["answerA3"] = objJson[k][key] :
                                            (curIndx - begAnswerIndx === 4) ? objGenerated[objGenerated.length - 1]["answerB"] = objJson[k][key] :
                                            (curIndx - begAnswerIndx === 5) ? objGenerated[objGenerated.length - 1]["trustA1"] = objJson[k][key] :
                                            (curIndx - begAnswerIndx === 6) ? objGenerated[objGenerated.length - 1]["trustA2"] = objJson[k][key] :
                                            (curIndx - begAnswerIndx === 7) ? objGenerated[objGenerated.length - 1]["trustA3"] = objJson[k][key] :
                                            objGenerated[objGenerated.length - 1]["trustB"] = objJson[k][key]
                                        }
                                    }
                                }
                                if (typeof objGenerated[objGenerated.length - 1] !== "undefined")
                                {
                                    console.log("adding differences; objGenerated[objGenerated.length - 1]: ",objGenerated[objGenerated.length - 1])
                                    // add differences
                                    if (typeof objGenerated[objGenerated.length - 1]["answerA1"]!=="undefined"){
                                        objGenerated[objGenerated.length - 1]["diffA1"] = objGenerated[objGenerated.length - 1]["bslnA1"]-objGenerated[objGenerated.length - 1]["answerA1"]
                                    } 
                                    if (typeof objGenerated[objGenerated.length - 1]["answerA2"]!=="undefined"){
                                        objGenerated[objGenerated.length - 1]["diffA2"] = objGenerated[objGenerated.length - 1]["bslnA2"]-objGenerated[objGenerated.length - 1]["answerA2"]
                                    } 
                                    if (typeof objGenerated[objGenerated.length - 1]["answerA3"]!=="undefined"){
                                        objGenerated[objGenerated.length - 1]["diffA3"] = objGenerated[objGenerated.length - 1]["bslnA3"]-objGenerated[objGenerated.length - 1]["answerA3"]
                                    } 
                                    if (typeof objGenerated[objGenerated.length - 1]["answerB"]!=="undefined"){
                                        var numDerivedB=(objGenerated[objGenerated.length - 1]["answerB"]=="Agree")?1:(objGenerated[objGenerated.length - 1]["answerB"]=="Neither agree nor disagree")?0:-1;
                                        objGenerated[objGenerated.length - 1]["correctB"] = 1 * objGenerated[objGenerated.length - 1]["blsnB"]==numDerivedB;
                                    }
                                    // if(strAttr!==null)objGenerated[objGenerated.length-1][strAttr]=objJson[k][key];
                                }
                            }
                        }

                    }
                }
            }

            console.log({ objGenerated });

            // const items = jsonObj;
            const items = objGenerated
            const replacer = (key, value) => value === null ? '' : value // specify how you want to handle null values here
            const header = Object.keys(items[0])
            console.log("header: ", header);
            const csvOutput = [
                header.join(','), // header row first
                ...items.map(row => header.map(fieldName => JSON.stringify(row[fieldName], replacer)).join(','))
            ].join('\r\n')

            // console.log(csvOutput)
            var t = new Date(); var fileNameTime = (addRandomInfoToFillVoid) ? "randomlyfilled_" + t.getTime() : t.getTime();
            console.log("writing " + "data/transformed/survey_precise-study_" + fileNameTime + ".csv")
            fs.writeFile("data/transformed/survey_precise-study_" + fileNameTime + ".csv", csvOutput, function (err, data) {
                if (err) console.log('error', err);
            });

        })


}


// New function: we need to establish the distribution of baselines.
function generateBaselineCSV(QIDtoFilename) {
    var hashmapBaselines = [];
    var objGenerated = []
    for (var qid in QIDtoFilename) {
        var cntrQ = QIDtoFilename[qid].split('-')[QIDtoFilename[qid].split('-').length - 1];
        var arrNames = QIDtoFilename[qid].split('-');
        if (typeof hashmapBaselines[cntrQ] === "undefined") {

            var bslnStr = QIDtoFilename[qid].split('-')[QIDtoFilename[qid].split('-').length - 2];
            var bslnArr = bslnStr.split('_');
            var parenthesisBsln = QIDtoFilename[qid].split('-')[QIDtoFilename[qid].split('-').length - 2].split('_')[1].split(')');
            var bslnA = Number(parenthesisBsln[0]), bslnB = Number(parenthesisBsln[1]), bslnC = Number(parenthesisBsln[2]);
            var bslnLikert = bslnArr[bslnArr.length - 1] === 't'

            var fcsSplit = QIDtoFilename[qid].split('-')[QIDtoFilename[qid].split('-').length - 3].split('_')
            var focusStr = "";
            if (typeof fcsSplit[2] === "undefined") { focusStr = fcsSplit[1] } else { focusStr = fcsSplit[1] + '_' + fcsSplit[2] }

            var maskDiff = arrNames[6].split('_')[1];
            var strAllFocusDiff = arrNames[7].split('_')[1]
            var focusDiff = "";
            focusDiff = (focusStr === "WHAT_Qn") ? strAllFocusDiff[0] : (focusStr === "WHAT_Ql") ? strAllFocusDiff[1] : strAllFocusDiff[2];
            focusDiff = (focusDiff === "E") ? "Easy" : (focusDiff === "M") ? "Medium" : "Hard;"

            hashmapBaselines[cntrQ] = { "bslnA": bslnA, "bslnB": bslnB, "bslnC": bslnC, "bslnLikert": bslnLikert, "focus": focusStr, "focusDiff": focusDiff, "maskDiff": maskDiff }

            objGenerated.push({})
            objGenerated[objGenerated.length - 1]["cntrQ"] = cntrQ, objGenerated[objGenerated.length - 1]["bslnA"] = bslnA, objGenerated[objGenerated.length - 1]["bslnB"] = bslnB, objGenerated[objGenerated.length - 1]["bslnC"] = bslnC,
                objGenerated[objGenerated.length - 1]["bslnLikert"] = bslnLikert, objGenerated[objGenerated.length - 1]["focus"] = focusStr, objGenerated[objGenerated.length - 1]["focusDiff"] = focusDiff,
                objGenerated[objGenerated.length - 1]["totalDiff"] = strAllFocusDiff, objGenerated[objGenerated.length - 1]["maskDiff"] = maskDiff;


        }
    }

    var itemsN = objGenerated;
    var replacerN = (key, value) => value === null ? '' : value // specify how you want to handle null values here
    var headerN = Object.keys(itemsN[0])
    console.log("headerN: ", headerN);
    var csvOutputN = [
        headerN.join(','), // header row first
        ...itemsN.map(row => headerN.map(fieldName => JSON.stringify(row[fieldName], replacerN)).join(','))
    ].join('\r\n')

    fs.writeFile("data/transformed/baselines.csv", csvOutputN, function (err, data) {
        if (err) console.log('error', err);
    });
}




// ---- Calls of the functions 
// generateModifiedCSV(QIDtoFilename,csvFilePath)
// generateModifiedCSV(QIDtoFilename,csvFilePath)

// Commented for tests, but should work
newGenerateModifiedCSV(QIDtoFilename, csvFilePath)

// generateBaselineCSV(QIDtoFilename)



