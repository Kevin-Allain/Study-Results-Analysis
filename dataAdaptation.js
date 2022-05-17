console.log(
    "---- dataAdaptation is a file that is implemented to be a cleaner version of dataTransform_final.js"
);

var $ = require("jquery");
const csvparser = require("csv-parser");
const fs = require("fs");
const csv = require("csvtojson");
const { FORMERR } = require("dns");
// ---- Data sources & global variables

var csvFilePathArray_noTikTok = "Data_2022_05_12/Sources/measurement_all_headerAdapted_flipWritten.csv";

// short terms to long attributes names
hashmapAttributesNames_glbl = {
    eG: "engine temperature",
    eT: "engine temperature",
    fC: "fuel consumption",
    eC: "electricity consumption",
    sF: "suspension force",
    gO: "gps on",
    wO: "wiper on",
    rO: "radio on",
    hO: "heating on",
}; // there was a bug with eT in the data generation, but fixed with this

var QIDtoFilename_nf = fs.readFileSync( "Data_2022_05_12/Sources/qid_nf_2022_05_12.json" );
QIDtoFilename_nf = JSON.parse(QIDtoFilename_nf);
var QIDtoFilename_f = fs.readFileSync( "Data_2022_05_12/Sources/qid_f_2022_05_12.json" );
QIDtoFilename_f = JSON.parse(QIDtoFilename_f);

// ---- Functions

Array.prototype.indicesOf = function(query) {
    var indices = [];
    var x = this.indexOf(query);
    while(x != -1) {
        indices.push(x);
        x = this.indexOf(query, x + 1);
    }
    return indices;
};


// This functions is called to generated a csv file listing the baselines extracted from QIDtoFilename_nf
function generateBaselineCSV(QIDtoFilename_nf,QIDtoFilename_f, additionNameBaseline = "") {
    console.log(
        "---- generateBaselineCSV, with additionNameBaseline: ",
        additionNameBaseline
    );
    if (additionNameBaseline === "") {
        additionNameBaseline = "_";
    }
    var hashmapBaselines = [];
    var objGenerated = [];
    for (var qid in QIDtoFilename_nf) {
        var cntrQ =
            QIDtoFilename_nf[qid].split("-")[QIDtoFilename_nf[qid].split("-").length - 1];
        var arrNames = QIDtoFilename_nf[qid].split("-");
        if (typeof hashmapBaselines[cntrQ] === "undefined") {
            var bslnStr =
                QIDtoFilename_nf[qid].split("-")[QIDtoFilename_nf[qid].split("-").length - 2];
            var bslnArr = bslnStr.split("_");
            var parenthesisBsln = QIDtoFilename_nf[qid]
                .split("-")
            [QIDtoFilename_nf[qid].split("-").length - 2].split("_")[1]
                .split(")");
            var bslnA = Number(parenthesisBsln[0]),
                bslnB = Number(parenthesisBsln[1]),
                bslnC = Number(parenthesisBsln[2]);
            var bslnLikert = bslnArr[bslnArr.length - 1] === "t";

            var fcsSplit = QIDtoFilename_nf[qid]
                .split("-")
            [QIDtoFilename_nf[qid].split("-").length - 3].split("_");
            var focusStr = "";
            if (typeof fcsSplit[2] === "undefined") {
                focusStr = fcsSplit[1];
            } else {
                focusStr = fcsSplit[1] + "_" + fcsSplit[2];
            }

            var maskDiff = arrNames[6].split("_")[1];
            var strAllFocusDiff = arrNames[7].split("_")[1];
            var focusDiff = "";
            focusDiff =
                focusStr === "WHAT_Qn"
                    ? strAllFocusDiff[0]
                    : focusStr === "WHAT_Ql"
                        ? strAllFocusDiff[1]
                        : strAllFocusDiff[2];
            focusDiff =
                focusDiff === "E" ? "Easy" : focusDiff === "M" ? "Medium" : "Hard";

            hashmapBaselines[cntrQ] = {
                bslnA: bslnA,
                bslnB: bslnB,
                bslnC: bslnC,
                bslnLikert: bslnLikert,
                focus: focusStr,
                focusDiff: focusDiff,
                maskDiff: maskDiff,
            };

            objGenerated.push({});
            (objGenerated[objGenerated.length - 1]["cntrQ"] = cntrQ),
                (objGenerated[objGenerated.length - 1]["bslnA"] = bslnA),
                (objGenerated[objGenerated.length - 1]["bslnB"] = bslnB),
                (objGenerated[objGenerated.length - 1]["bslnC"] = bslnC),
                (objGenerated[objGenerated.length - 1]["bslnLikert"] = bslnLikert),
                (objGenerated[objGenerated.length - 1]["focus"] = focusStr),
                (objGenerated[objGenerated.length - 1]["focusDiff"] = focusDiff),
                (objGenerated[objGenerated.length - 1]["totalDiff"] = strAllFocusDiff),
                (objGenerated[objGenerated.length - 1]["maskDiff"] = maskDiff);
        }
    }

    for (var qid in QIDtoFilename_f) {
        var cntrQ =
            QIDtoFilename_f[qid].split("-")[QIDtoFilename_f[qid].split("-").length - 1];
        var arrNames = QIDtoFilename_f[qid].split("-");
        if (typeof hashmapBaselines[cntrQ] === "undefined") {
            var bslnStr =
                QIDtoFilename_f[qid].split("-")[QIDtoFilename_f[qid].split("-").length - 2];
            var bslnArr = bslnStr.split("_");
            var parenthesisBsln = QIDtoFilename_f[qid]
                .split("-")
            [QIDtoFilename_f[qid].split("-").length - 2].split("_")[1]
                .split(")");
            var bslnA = Number(parenthesisBsln[0]),
                bslnB = Number(parenthesisBsln[1]),
                bslnC = Number(parenthesisBsln[2]);
            var bslnLikert = bslnArr[bslnArr.length - 1] === "t";
    
            var fcsSplit = QIDtoFilename_f[qid]
                .split("-")
            [QIDtoFilename_f[qid].split("-").length - 3].split("_");
            var focusStr = "";
            if (typeof fcsSplit[2] === "undefined") {
                focusStr = fcsSplit[1];
            } else {
                focusStr = fcsSplit[1] + "_" + fcsSplit[2];
            }
    
            var maskDiff = arrNames[6].split("_")[1];
            var strAllFocusDiff = arrNames[7].split("_")[1];
            var focusDiff = "";
            focusDiff =
                focusStr === "WHAT_Qn"
                    ? strAllFocusDiff[0]
                    : focusStr === "WHAT_Ql"
                        ? strAllFocusDiff[1]
                        : strAllFocusDiff[2];
            focusDiff =
                focusDiff === "E" ? "Easy" : focusDiff === "M" ? "Medium" : "Hard";
    
            hashmapBaselines[cntrQ] = {
                bslnA: bslnA,
                bslnB: bslnB,
                bslnC: bslnC,
                bslnLikert: bslnLikert,
                focus: focusStr,
                focusDiff: focusDiff,
                maskDiff: maskDiff,
            };
    
            objGenerated.push({});
            (objGenerated[objGenerated.length - 1]["cntrQ"] = cntrQ),
                (objGenerated[objGenerated.length - 1]["bslnA"] = bslnA),
                (objGenerated[objGenerated.length - 1]["bslnB"] = bslnB),
                (objGenerated[objGenerated.length - 1]["bslnC"] = bslnC),
                (objGenerated[objGenerated.length - 1]["bslnLikert"] = bslnLikert),
                (objGenerated[objGenerated.length - 1]["focus"] = focusStr),
                (objGenerated[objGenerated.length - 1]["focusDiff"] = focusDiff),
                (objGenerated[objGenerated.length - 1]["totalDiff"] = strAllFocusDiff),
                (objGenerated[objGenerated.length - 1]["maskDiff"] = maskDiff);
        }
    }
    

    // let's write according to date
    var d = new Date();
    var strTime =
        d.getFullYear() +
        "_" +
        (d.getMonth() + 1) +
        "_" +
        d.getUTCDate() +
        "_" +
        d.getHours();

    var itemsN = objGenerated;
    var replacerN = (key, value) => (value === null ? "" : value); // specify how you want to handle null values here
    var headerN = Object.keys(itemsN[0]);
    console.log("headerN: ", headerN);
    var csvOutputN = [
        headerN.join(","), // header row first
        ...itemsN.map((row) =>
            headerN
                .map((fieldName) => JSON.stringify(row[fieldName], replacerN))
                .join(",")
        ),
    ].join("\r\n");

    console.log(
        "writing : " +
        "Data_2022_05_12/Intermediary/baselines/baselines" +
        additionNameBaseline +
        strTime +
        ".csv"
    );
    fs.writeFile(
        "Data_2022_05_12/Intermediary/baselines/baselines" +
        additionNameBaseline +
        strTime +
        ".csv",
        csvOutputN,
        function (err, data) {
            if (err) console.log("error", err);
        }
    );
}

function writeNoFilterWrongAnswersToIntro(csvFilePath, fileEnd = "") {
    console.log(
        "---- writeNoFilterWrongAnswersToIntro, csvFilePath: ",
        csvFilePath,
        ", fileEnd: ",
        fileEnd
    );
    var fileStudyBit = csvFilePath
        .split("/")
    [csvFilePath.split("/").length - 1].substring(
        0,
        csvFilePath.split("/")[csvFilePath.split("/").length - 1].length - 4
    );
    console.log("fileStudyBit: ", fileStudyBit);
    var baselinesIntro = {
        Q11: "Down",
        Q12: "From time 10 to 30 and time 60 to 70",
        Q13: "Time 30",
        Q17: "On the first long straight section", // Is this the right one?!
        Q14: "From a little before time 70 up to a little before time 80",
    };
    var objGenerated = [];
    const toFilterProlificIds = [];
    glbl_toFilterProlificIds = [];
    hashmap_toKeepProlificIds = {};
    arr_passing_filters = []; // Making an array where for each response we store responseId, prolificId, and whether introduction questions are correct.
    var rArr = csv()
        .fromFile(csvFilePath)
        .then((objJson) => {
            console.log("loaded the file csvFilePath: ", csvFilePath,", objJson[0]['Q11']: ",objJson[0]["Q11"],", objJson[0]['Q12']: ",objJson[0]["Q12"],", objJson[0]['Q13']: ",objJson[0]["Q13"],", objJson[0]['Q17']: ",objJson[0]["Q17"],", objJson[0]['Q14']: ",objJson[0]["Q14"]);
            for (var k in objJson) {
                if (k >= 0) {
                    if (objJson[k]["Q15"] === "jsndyks"){ console.log("For Jason objJson[k]['Q11']: ",objJson[k]["Q11"],", objJson[k]['Q12']: ",objJson[k]["Q12"],", objJson[k]['Q13']: ",objJson[k]["Q13"],", objJson[k]['Q17']: ",objJson[k]["Q17"],", objJson[k]['Q14']: ",objJson[k]["Q14"]);}
                    var passedIntroQuestions = objJson[k]["Q11"] === baselinesIntro["Q11"] && objJson[k]["Q12"] === baselinesIntro["Q12"] && objJson[k]["Q13"] === baselinesIntro["Q13"] && objJson[k]["Q17"] === baselinesIntro["Q17"] && objJson[k]["Q14"] === baselinesIntro["Q14"];
                    var responseId = objJson[k]["ResponseId"];
                    var finished = objJson[k]["Finished"] === "TRUE";
                    var flip = objJson[k]["flip"];
                    var responseWrittenAlready = typeof( arr_passing_filters.find(a => a.responseId === objJson[k]["ResponseId"])) !== "undefined";
                    if (passedIntroQuestions && !responseWrittenAlready) {                                
                        arr_passing_filters.push({ prolificId: objJson[k]["Q15"].toString(), ResponseId: responseId, passedIntroQuestions: passedIntroQuestions,finished:finished, flip:flip });
                    } else if (!responseWrittenAlready) {
                        arr_passing_filters.push({ prolificId: objJson[k]["Q15"].toString(), ResponseId: responseId, passedIntroQuestions: passedIntroQuestions,finished:finished, flip: flip });
                    }
                }
            }
            var d = new Date();
            var strTime = d.getFullYear() + "_" + (d.getMonth() + 1) + "_" + d.getUTCDate() + "_" + d.getHours();

            csvFilePath = csvFilePath.split("/")[csvFilePath.split("/").length - 1].substring(0,csvFilePath.split("/")[csvFilePath.split("/").length - 1].length - 4);
            var toFilter_JSONpath ="Data_2022_05_12/Intermediary/PassingFilters_" +csvFilePath +"_" +strTime +".json";
            console.log("writing ", toFilter_JSONpath);
            fs.writeFile(
                toFilter_JSONpath,
                JSON.stringify(arr_passing_filters),
                function (err, data) {
                    if (err) console.log("error", err);
                }
            );
            return toFilterProlificIds;
        });
    return toFilterProlificIds;
}

function extractColumnsFromFilename(filename, hashmapAttributesNames = hashmapAttributesNames_glbl) {
    // console.log("extractColumnsFromFilename: ",filename);
    var idc = Number(filename.substr("idc".length, filename.indexOf('-') - "idc".length));
    var splitFN = filename.split('-');
    var drawnQn = hashmapAttributesNames[splitFN[1]];
    var drawnQl = hashmapAttributesNames[splitFN[2]];
    var queryArr = splitFN[3].split('_');
    // console.log("for queryString... hashmapAttributesNames: ",hashmapAttributesNames+", queryArr[1]: ",queryArr[1],", queryArr[2]: ",queryArr[2],", queryArr[3]: ",queryArr[3],", splitFN: ",splitFN)
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
        bslnA1 = Number(strSplitBlsn[0]);
        bslnA2 = Number(strSplitBlsn[1]);
        bslnA3 = Number(strSplitBlsn[2]);
    } else {
        bslnA = (splitFN[9].substr(splitFN[9].indexOf('_') + 1, 1) === 't') ? 1 : -1;
    }
    var bslnB = (splitFN[9].substr(splitFN[9].length - 1) === 't') ? 1 : -1;
    var cntrQ = Number(splitFN[10].substr(splitFN[10].indexOf('_') + 1));

    // console.log({idc,drawnQn, drawnQl, queryString,flips, nMasks, dMask, dComplex_Qn, dComplex_Ql,dComplex_Where,focus, bslnA, bslnB,cntrQ})

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

function newNoFilterGenerateModifiedCSV(csvFilePath, QIDtoFilename_nf, QIDtoFilename_f,  fileDataFilterIntro="", addRandomInfoToFillVoid = false,  fileEnd="") {
    console.log("---- newGenerateModifiedCSV ",{csvFilePath,addRandomInfoToFillVoid})

    var cptQID = 0;
    for(var k in QIDtoFilename_nf){
        if ( cptQID%243 === 0){
            console.log("QIDtoFilename_nf[",k,"]: ",QIDtoFilename_nf[k]);
        }
        cptQID++;
    }

    var objGenerated = [];
    // var cptForBundlingQuestions = 0; var storeResponseId = ""; var storeStartDate = "", storeEndDate = "", storeProgress = "", storeDuration_in_seconds = "", storeFinished = "", storeRecordedDate = ""; var baseIndx_frstClck = 0;
    var filesInfo = null;
    if ( fileDataFilterIntro === "") { 
         fileDataFilterIntro = "Data_2022_05_12/Intermediary/PassingFilters_measurement_all_headerAdapted_flipWritten_2022_5_12_8.json"
        filesInfo = fs.readFileSync(fileDataFilterIntro);
        filesInfo = JSON.parse(filesInfo);
    }
    console.log("trying to load csvFilePath: ",csvFilePath,",  fileDataFilterIntro: ", fileDataFilterIntro);
    console.log("filesInfo[0]: ", filesInfo[0])
    csv()
        .fromFile(csvFilePath)
        .then((objJson) => {
            console.log("loading done, csvFilePath: ",csvFilePath);
            // indx for values which are shared for all the participants. mod is for answers over several questions
            var indxStartDate = 1, indxEndDate = 2, indxStatus = 3, indxProgress = 5, indxDuration_seconds = 6, indxFinished = 7, indxRecordedDate = 8, indxResponseID = 9;
            var fileName = null, idc = null, drawnQn = null, drawnQl = null, queryString = null, flips = null, nMasks = null, dMask = null, dComplex_Qn = null, dComplex_Ql = null, dComplex_Where = null, focus = null, bslnA1 = null, bslnA2 = null, bslnA3 = null, bslnB = null, cntrQ = null;
            var modAnswerTime = null, modAnswerA1 = null, modAnswerA2 = null, modAnswerA3 = null, modAnswerB = null, modTrustA1 = null, modTrustA2 = null, modTrustA3 = null, modTrustB = null
            // load the names of files to filter
            var arrayDataToKeep = fs.readFileSync( fileDataFilterIntro,'utf8');
            arrayDataToKeep = JSON.parse(arrayDataToKeep);
            var uniqueCntrQuestion = 0;

            /**
             * generating artificial prolificId for responses without it
             */
            // var arrRespNoProlific = [];
            // var j=0;
            // for(var k in objJson){
            //     var curProlificId = objJson[k]["Q15"];
            //     if(curProlificId===""){
            //         arrRespNoProlific.push(objJson[k]["ResponseId"])
            //         var strProlifAdd = "artifProlif"+j;
            //         arrayDataToKeep = arrayDataToKeep.map(o => {
            //             if(o.ResponseId===objJson[k]["ResponseId"]){
            //                 return {...o, prolificId:strProlifAdd}
            //             }
            //             return o;
            //         })
            //         j++
            //     }
            // }
            // for(var i=0; i<arrRespNoProlific.length;i++){
            //     var numProlifArtif = "artifProlif"+i;
            //     objJson = objJson.map(o => {
            //         if(o.ResponseId === arrRespNoProlific[i]){
            //             return {...o, Q15:numProlifArtif}
            //         }
            //         return o;
            //     })
            // }
            
            for (var k in objJson) {
                if (k >= 0) {
                    // Verify if not an object to filter out!
                    var curProlificId = objJson[k]["Q15"];
                    var indexArrDataToKeep = arrayDataToKeep.findIndex(a => a.prolificId === curProlificId);
                    var curFlip = objJson[k]["flip"];
                    console.log("curProlificId: ",curProlificId,',passedIntroQuestions: ',arrayDataToKeep[indexArrDataToKeep]["passedIntroQuestions"]===true , ', Finished: ',arrayDataToKeep[indexArrDataToKeep]["Finished"]===true );

                    if ( 
                        indexArrDataToKeep !== -1 
                        // && arrayDataToKeep[indexArrDataToKeep]["passedIntroQuestions"]===true 
                        // && arrayDataToKeep[indexArrDataToKeep]["finished"]===true 
                        )
                    {
                        // Keep a boolean to indicate if the QID has a page submit...
                        var begAnswerIndx = null;
                        var firstAnswerGenerated = false;
                        var objShared = {};
                        for (var key in objJson[k]) {
                                if (objJson[k][key] !== '') {
                                    if (key[0] !== "Q") {
                                        objShared[key] = objJson[k][key]
                                    } else {
                                        var curIndx = (key.indexOf('_') !== -1) ? Number(key.substr(1, key.indexOf('_') - 1)) : Number(key.substr(1));
                                        if (curIndx > 23) {
                                            var keyQual = "QID" + curIndx;
                                            // console.log("curIndx: ",curIndx,", QIDtoFilename_nf: ",QIDtoFilename_nf);
                                            var filename = (curFlip==="f")?QIDtoFilename_f[keyQual]:QIDtoFilename_nf[keyQual];
                                            if (key.indexOf("First") !== -1) {
                                                var objInfo = extractColumnsFromFilename(filename);
                                                objGenerated.push({}); 
                                                // console.log("found the first part. ", { objInfo }, { objShared })
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
                                                objGenerated[objGenerated.length - 1]["dComplex_focus"] = (objGenerated[objGenerated.length - 1]["focus"]==="WHAT_Qn")?objGenerated[objGenerated.length - 1]["dComplex_Qn"] :  (objGenerated[objGenerated.length - 1]["focus"]==="WHAT_Ql")?objGenerated[objGenerated.length - 1]["dComplex_Ql"] : objGenerated[objGenerated.length - 1]["dComplex_Where"] 
                                                objGenerated[objGenerated.length - 1]["info_focus_dComplex_dMask"] = objGenerated[objGenerated.length - 1]["focus"]+"_"+objGenerated[objGenerated.length - 1]["dComplex_focus"]+"_"
                                                    + ((objGenerated[objGenerated.length - 1]["dMask"] == "easy")?"E":(objGenerated[objGenerated.length - 1]["dMask"] == "medium")?"M":"H");

                                                objGenerated[objGenerated.length - 1]["info_dComplex_dMask"] = objGenerated[objGenerated.length - 1]["dComplex_focus"]+"_"
                                                    + ((objGenerated[objGenerated.length - 1]["dMask"] == "easy")?"E":(objGenerated[objGenerated.length - 1]["dMask"] == "medium")?"M":"H");
                                            }
                                            if (key.indexOf("Submit") !== -1) {
                                                begAnswerIndx = curIndx;
                                                objGenerated[objGenerated.length - 1]['t'] = objJson[k][key];
                                            } else {
                                                if (curIndx > begAnswerIndx) {
                                                    (curIndx - begAnswerIndx === 1) ? objGenerated[objGenerated.length - 1]["answerA1"] = objJson[k][key] :
                                                    (curIndx - begAnswerIndx === 2) ? objGenerated[objGenerated.length - 1]["answerA2"] = objJson[k][key] :
                                                    (curIndx - begAnswerIndx === 3) ? objGenerated[objGenerated.length - 1]["answerA3"] = objJson[k][key] :
                                                    (curIndx - begAnswerIndx === 4) ? objGenerated[objGenerated.length - 1]["answerB"] = objJson[k][key] :
                                                    (curIndx - begAnswerIndx === 5) ? objGenerated[objGenerated.length - 1]["trustA1"] = objJson[k][key] :
                                                    (curIndx - begAnswerIndx === 6) ? objGenerated[objGenerated.length - 1]["trustA2"] = objJson[k][key] :
                                                    (curIndx - begAnswerIndx === 7) ? objGenerated[objGenerated.length - 1]["trustA3"] = objJson[k][key] :
                                                    objGenerated[objGenerated.length - 1]["trustB"] = objJson[k][key]
                                                    if(curIndx-begAnswerIndx === 8 )
                                                    {
                                                        objGenerated[objGenerated.length - 1]["flip"]=curFlip;
                                                        objGenerated[objGenerated.length - 1]["passedIntroQuestions"]= arrayDataToKeep[indexArrDataToKeep]["passedIntroQuestions"];
                                                        objGenerated[objGenerated.length - 1]["prolificId"]= arrayDataToKeep[indexArrDataToKeep]["prolificId"];
                                                        // objGenerated[objGenerated.length - 1]["prolificId"]=...;

                                                        // Are there impossible qualitative answers?
                                                        objGenerated[objGenerated.length - 1]["impossibleQualAnswer"] = null;
                                                        if(objGenerated[objGenerated.length - 1]["focus"]==="WHAT_Ql")
                                                        {
                                                            objGenerated[objGenerated.length - 1]["impossibleQualAnswer"] = (Number(objGenerated[objGenerated.length - 1]["answerA1"]) > Number(objGenerated[objGenerated.length - 1]["answerA2"]))? true : false;
                                                        }
                                                        // Are there responses where all the trust reported is 0 or 5
                                                        if (objGenerated[objGenerated.length - 1]["prolificId"] === "0xd07c586e8691aa3D2086F26ACF7ad183adCf37b6"){
                                                            // console.log('Number(objGenerated[objGenerated.length - 1]["trustA1"]): ',Number(objGenerated[objGenerated.length - 1]["trustA1"]) )
                                                            // console.log('Number(objGenerated[objGenerated.length - 1]["trustA2"]): ',Number(objGenerated[objGenerated.length - 1]["trustA2"]) )
                                                            // console.log('Number(objGenerated[objGenerated.length - 1]["trustA3"]): ',Number(objGenerated[objGenerated.length - 1]["trustA3"]) )
                                                            // console.log('Number(objGenerated[objGenerated.length - 1]["trustB"]): ',Number(objGenerated[objGenerated.length - 1]["trustB"]) )
                                                            // console.log('Number(objGenerated[objGenerated.length - 1]["trustA1"])===Number(objGenerated[objGenerated.length - 1]["trustA2"]: ',Number(objGenerated[objGenerated.length - 1]["trustA1"])===Number(objGenerated[objGenerated.length - 1]["trustA2"]))
                                                        }
                                                        if( Number(objGenerated[objGenerated.length - 1]["trustA1"])===Number(objGenerated[objGenerated.length - 1]["trustA2"]) 
                                                            && Number(objGenerated[objGenerated.length - 1]["trustA2"])===Number(objGenerated[objGenerated.length - 1]["trustA3"])
                                                            && Number(objGenerated[objGenerated.length - 1]["trustA3"])===Number(objGenerated[objGenerated.length - 1]["trustB"]))
                                                        {
                                                            objGenerated[objGenerated.length - 1]["stimuliAllTrust0or5"] = (Number(objGenerated[objGenerated.length - 1]["trustA1"]) === 0 || Number(objGenerated[objGenerated.length - 1]["trustA1"]) === 5)?true:false;
                                                        } else {
                                                            objGenerated[objGenerated.length - 1]["stimuliAllTrust0or5"] = false;
                                                        }                                                     
                                                        objGenerated[objGenerated.length - 1]["responseAllTrust0or5"] = false;

                                                        objGenerated[objGenerated.length-1]["uniqueCntrQuestion"] = uniqueCntrQuestion;
                                                        uniqueCntrQuestion++;
                                                    }
                                                }
                                            }
                                        }
                                        if (typeof objGenerated[objGenerated.length - 1] !== "undefined")
                                        {
                                            if (typeof objGenerated[objGenerated.length - 1]["answerA1"]!=="undefined"){
                                                objGenerated[objGenerated.length - 1]["diffA1"] = objGenerated[objGenerated.length - 1]["bslnA1"]-objGenerated[objGenerated.length - 1]["answerA1"]
                                                objGenerated[objGenerated.length - 1]["log_diffA1"] = Math.log2( Math.abs(objGenerated[objGenerated.length - 1]["bslnA1"]-objGenerated[objGenerated.length - 1]["answerA1"]) +1/8 )
                                            } 
                                            if (typeof objGenerated[objGenerated.length - 1]["answerA2"]!=="undefined"){
                                                objGenerated[objGenerated.length - 1]["diffA2"] = objGenerated[objGenerated.length - 1]["bslnA2"]-objGenerated[objGenerated.length - 1]["answerA2"]
                                                objGenerated[objGenerated.length - 1]["log_diffA2"] = Math.log2( Math.abs(objGenerated[objGenerated.length - 1]["bslnA2"]-objGenerated[objGenerated.length - 1]["answerA2"]) +1/8 )
                                            } 
                                            if (typeof objGenerated[objGenerated.length - 1]["answerA3"]!=="undefined"){
                                                objGenerated[objGenerated.length - 1]["diffA3"] = objGenerated[objGenerated.length - 1]["bslnA3"]-objGenerated[objGenerated.length - 1]["answerA3"]
                                                objGenerated[objGenerated.length - 1]["log_diffA3"] = Math.log2( Math.abs(objGenerated[objGenerated.length - 1]["bslnA3"]-objGenerated[objGenerated.length - 1]["answerA3"]) +1/8 )
                                            } 
                                            if (typeof objGenerated[objGenerated.length - 1]["answerB"]!=="undefined"){
                                                var numDerivedB=(objGenerated[objGenerated.length - 1]["answerB"]=="Agree")?1:(objGenerated[objGenerated.length - 1]["answerB"]=="Neither agree nor disagree")?0:-1;
                                                // console.log("numDerivedB: ",numDerivedB, ', objGenerated[objGenerated.length - 1]["bslnB"]: ',objGenerated[objGenerated.length - 1]["bslnB"], ', calculation: ', (1 * objGenerated[objGenerated.length - 1]["bslnB"]==numDerivedB) )
                                                objGenerated[objGenerated.length - 1]["correctB"] = 1 * (objGenerated[objGenerated.length - 1]["bslnB"]==numDerivedB);
                                            }

                                            // if (typeof objGenerated[objGenerated.length - 1]["trustA1"]!=="undefined" && typeof objGenerated[objGenerated.length - 1]["trustA2"]!=="undefined" && typeof objGenerated[objGenerated.length - 1]["trustA3"]!=="undefined" && typeof objGenerated[objGenerated.length - 1]["trustB"]!=="undefined"){  }

                                        }
                                    }
                                }                            
                        }
                    } else {
                        console.log("ignoring the answer from the prolific id: ", curProlificId)
                    }
                }
            }
            // console.log({ objGenerated });
            var cptForTrust0or5 = 0, prevRespId=null, currRespId=null, countStimuliPerResponses=0;
            for (var k in objGenerated){
                currRespId=objGenerated[k]["ResponseId"];
                if( objGenerated[k]["prolificId"]==="justine" || objGenerated[k]["ResponseId"]==="R_2QyMFtwSJcGPDat"){
                    console.log("justine objGenerated[k]: ",objGenerated[k]);
                    prevRespId=currRespId;
                }
                if(prevRespId!==currRespId){
                    var objFilPerResp = objGenerated.filter(a => a.ResponseId === currRespId)
                    if ( objFilPerResp.length === 9 ){
                        // console.log("RIGHT NUMBER OF RESPONSES: cptForTrust0or5%9===0. objGenerated[k]['ResponseId']: ",objGenerated[k]['ResponseId']);
                        var allTrust0 = true, allTrust5=true;
                        for(var l in objFilPerResp){
                            if(
                                !(Number(objFilPerResp[l]["trustA1"]===0
                                &&Number(objFilPerResp[l]["trustA2"])===0
                                &&Number(objFilPerResp[l]["trustA3"])===0
                                &&Number(objFilPerResp[l]["trustB"])===0))
                                ){
                                    allTrust0=false;
                            }
                            if(
                                !(Number(objFilPerResp[l]["trustA1"]===5
                                &&Number(objFilPerResp[l]["trustA2"])===5
                                &&Number(objFilPerResp[l]["trustA3"])===5
                                &&Number(objFilPerResp[l]["trustB"])===5))
                                ){
                                    allTrust5=false;
                            }
                        }
                        if(allTrust0 || allTrust5){
                            // console.log("Need to set true for allTrust0or5 for ResponseId",objFilPerResp[0].ResponseId);
                        }
                    } else { // weirdly not called...?!
                        // console.log("WRONG NUMBER OF RESPONSES: cptForTrust0or5%9===0. objGenerated[k]['ResponseId']: ",objGenerated[k]['ResponseId'],", objGenerated[k]['prolificId']: ",objGenerated[k]['prolificId'],", objGenerated[k]['flip']: ",objGenerated[k]['flip']);
                    }
                    prevRespId=currRespId;
                }
                cptForTrust0or5++;
                countStimuliPerResponses++;
            }

            const items = objGenerated
            const replacer = (key, value) => value === null ? '' : value // specify how you want to handle null values here
            const header = Object.keys(items[0])
            const csvOutput = [
                header.join(','),
                ...items.map(row => header.map(fieldName => JSON.stringify(row[fieldName], replacer)).join(','))
            ].join('\r\n')

            // let's write according to date
            var d = new Date();
            var strTime = d.getFullYear()+"_"+(d.getMonth()+1)+"_"+d.getUTCDate()+"_"+d.getHours();

            var fileStudyBit = csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4);            
            var t = new Date(); var fileNameTime = (addRandomInfoToFillVoid) ? "randomlyfilled_" + t.getTime() : fileStudyBit;
            
            
            // Addition to include the issueStabilityComparison attribute and stablityComparison (there were issues...)

            
            console.log("writing " + "Data_2022_05_12/Outputs/noFilter_survey_" + fileNameTime + fileEnd +"_"+strTime+ ".csv")
            fs.writeFile("Data_2022_05_12/Outputs/noFilter_survey_" + fileNameTime +fileEnd+"_"+strTime + ".csv", csvOutput, function (err, data) { if (err) console.log('error', err); });
        })
}

/**
 * The calls to functions are based with the following logic:
 * - Recover the baselines in the generateBaselineCSV function and write that in a baselines file
 * - The function writeNoFilterWrongAnswersToIntro is used to write indications of participants who did not pass the first filter
 */

// // ======== Prior to TikTok trend
// // ---- Generate baselines -- fluent & first language
// console.log("---- About to generate baselines")
// generateBaselineCSV(QIDtoFilename_nf,QIDtoFilename_f);

// // ---- Filter out answers with wrong intro // seems ok on 2021_11_03
// console.log("---- Filter out answers with wrong intro.");
// writeNoFilterWrongAnswersToIntro(csvFilePathArray_noTikTok); // to use if we wish outputs without filters (can we set that information directly here...?)

// // // ---- Generate structured files of answers
// console.log("---- Generate structured files of answers, csvFilePathArray_noTikTok: ",csvFilePathArray_noTikTok)
console.log("csvFilePathArray_noTikTok: ",csvFilePathArray_noTikTok);
newNoFilterGenerateModifiedCSV( csvFilePathArray_noTikTok,QIDtoFilename_nf,QIDtoFilename_f)
//     //newGenerateModifiedCSV (listQIDtoFileName_noTikTok_withJason[i], csvFilePathArray_withJason[i], "" , false, "_withJason_withLog2" )
