console.log("---- dataTransform.js")
// https://paper.dropbox.com/doc/Notes-about-coding-for-results-analysis--BIcFyz4C8sfOP9sNxjJhTANNAQ-2QtCwBvX4Vng9cWFJjlds
var $ = require("jquery");
const csvparser = require('csv-parser');
const fs = require('fs');
// I'm going to transform the csv into json, modify it accordingly, and then create a new csv.
const csv = require('csvtojson')
console.log(Math.random())
// ---- Data sources & global variables
// const csvFilePath='data/survey_precise-study_20210410_1500_April 10, 2021_07.36.csv' // const csvFilePath='data/survey_precise-study_20210410_1500_April 10, 2021_07.36_modified.csv' // slider_noflip_rightParenthesisBsln_headerReduced // var csvFilePath='data/slider_noflip_rightParenthesisBsln_headerReduced.csv' // var csvFilePath='data/FullStudy_final_precise_noflip_automatedAnswers.csv' // var csvFilePath='data/test_reordered_question_20210721_1547.csv'; // var csvFilePath="data/js_update_questions_reordereed_headFiltered_20210722_1136.csv" // var csvFilePath = "data/new_generation_processing_headFiltered_20210730_1220.csv" // var csvFilePath = "data/t16720211305_alt_focus_modif.csv" // var csvFilePath = 'data/t26720211035_attempt_flip1_header_adapted.csv'
var csvFilePath = 'data/complete_20210828_noflip_headerAdapted.csv'

// Added the _MMM_replaced for some elements that were missing a category
// Update for First Language English
// complete_measurement_f_2021_09_18_October_22_headerAdapted
// complete_measurement_nf_2021_09_18_October_22_headerAdapted
var csvFilePathArray_fluent = [];
csvFilePathArray_fluent.push(
    "data/participants_answers_headerAdapted_untransformed/complete_measurement_nf_2021_09_18_headerAdapted.csv",
    "data/participants_answers_headerAdapted_untransformed/complete_measurement_f_2021_09_18_headerAdapted.csv",
    "data/participants_answers_headerAdapted_untransformed/complete_distractor_h_2021_09_18_headerAdapted_MMM_replaced.csv",
    "data/participants_answers_headerAdapted_untransformed/complete_distractor_n_2021_09_18_headerAdapted_MMM_replaced.csv",
    "data/participants_answers_headerAdapted_untransformed/complete_scaling_0_2021_09_19_headerAdapted_MMM_replaced.csv",
    "data/participants_answers_headerAdapted_untransformed/complete_scaling_1_2021_09_19_headerAdapted_MMM_replaced.csv", 
    "data/participants_answers_headerAdapted_untransformed/complete_scaling_2_2021_09_19_headerAdapted_MMM_replaced.csv"
)
var csvFilePathArray_FirstLanguage = [];
csvFilePathArray_FirstLanguage.push(
    "data/participants_answers_headerAdapted_untransformed/complete_measurement_f_2021_09_18_October_22_headerAdapted.csv",
    "data/participants_answers_headerAdapted_untransformed/complete_measurement_nf_2021_09_18_October_22_headerAdapted.csv",
    "data/participants_answers_headerAdapted_untransformed/full_distractor_h_headerAdapted.csv",
    "data/participants_answers_headerAdapted_untransformed/full_distractor_n_headerAdapted.csv",
    "data/participants_answers_headerAdapted_untransformed/full_scaling_0_headerAdapted.csv",
    "data/participants_answers_headerAdapted_untransformed/full_scaling_1_headerAdapted.csv",
    "data/participants_answers_headerAdapted_untransformed/full_scaling_2_headerAdapted.csv"
)

var csvFilePathArray_noTikTok = [];
csvFilePathArray_noTikTok.push(
    "Studies_2021_11_12/Results/measurement_f_headerAdapted.csv",
    "Studies_2021_11_12/Results/measurement_nf_headerAdapted.csv",
    "Studies_2021_11_12/Results/distractor_h_headerAdapted.csv",
    "Studies_2021_11_12/Results/distractor_n_headerAdapted.csv",
    "Studies_2021_11_12/Results/scaling_0_headerAdapted.csv",
    "Studies_2021_11_12/Results/scaling_1_headerAdapted.csv",
    "Studies_2021_11_12/Results/scaling_2_headerAdapted.csv"
)

// short terms to long attributes names
hashmapAttributesNames_glbl = {
    "eG": "engine temperature",
    "eT": "engine temperature", // there was a bug in the data generation, but not a big issue
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
    console.log("extractColumnsFromFilename: ",filename);
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

/* Return not working due to asynchonisity of read in csv. We write the file instead. Put a promise could be done, but we are in a big rush. */
function writeFilterWrongAnswersToIntro(csvFilePath, endFileAddition=""){
    console.log("---- filterWrongAnswersToIntro, csvFilePath: ",csvFilePath)
    var fileStudyBit = csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4);            

    var baselinesIntro = {
        "Q11":"Down",
        "Q12": "From time 10 to 30 and time 60 to 70",
        "Q13":"Time 30",
        "Q17":"On the first long straight section",
        "Q14":"From a little before time 70 up to a little before time 80"
    }
    var objGenerated = [];
    const toFilterProlificIds = [];
    glbl_toFilterProlificIds = [];
    hashmap_toKeepProlificIds = {};
    var rArr = csv()
        .fromFile(csvFilePath)
        // .complete((objJson) => { console.log("in complete part. objJson: ",objJson) })
        .then((objJson) => {
            console.log("loaded the file csvFilePath: ",csvFilePath)
            for (var k in objJson) {
                if (objJson[k]["ResponseId"] == "R_2QA1LGG7XnMia3O"){
                    console.log("should remove this one. objJson[k]: ",objJson[k])
                }
                if (k >= 0) {
                    for (var key in objJson[k]) {
                        if (objJson[k][key] !== '') {
                            if (key == "Q11" || key == "Q12"  || key == "Q13" || key == "Q17" || key == "Q14")
                            {
                                if (objJson[k][key] != baselinesIntro[key])
                                {
                                    // console.log("key: ",key,", objJson[k][key]: ",objJson[k][key],", baselinesIntro[key]: ",baselinesIntro[key],", objJson[k][key] != baselinesIntro[key]: ",(objJson[k][key] != baselinesIntro[key]),', objJson[k]["Q15"]: ',objJson[k]["Q15"])
                                    // Get the prolific id...
                                    if (toFilterProlificIds.indexOf(objJson[k]["Q15"]) == -1 )
                                    {
                                        toFilterProlificIds.push(objJson[k]["Q15"].toString())
                                        glbl_toFilterProlificIds.push(objJson[k]["Q15"].toString())
                                        // console.log("toFilterProlificIds: ",toFilterProlificIds)
                                    }

                                    hashmap_toKeepProlificIds[objJson[k]["Q15"].toString()] = false;

                                } else {
                                    if ( typeof(hashmap_toKeepProlificIds[objJson[k]["Q15"].toString()]) === "undefined" || hashmap_toKeepProlificIds[objJson[k]["Q15"].toString()]!=false )
                                    {
                                        hashmap_toKeepProlificIds[objJson[k]["Q15"].toString()] = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            console.log("endOf then. toFilterProlificIds: ",toFilterProlificIds)

            csvFilePath = csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4);            
            var toFilter_JSONpath = "data/toFilter/toFilter_"+csvFilePath+endFileAddition+".json"
            // console.log("hashmap_toKeepProlificIds: ",hashmap_toKeepProlificIds,", toFilter_JSONpath: ", toFilter_JSONpath)
            fs.writeFile(toFilter_JSONpath, JSON.stringify(hashmap_toKeepProlificIds), function (err, data) {
                if (err) console.log('error', err);
            });

            return toFilterProlificIds     
        })
    // console.log("!!! toFilterProlificIds: ",toFilterProlificIds, ", glbl_toFilterProlificIds: ",glbl_toFilterProlificIds,", hashmap_toKeepProlificIds: ",hashmap_toKeepProlificIds,", rArr: ",rArr)
    return toFilterProlificIds;
}

function writeNoFilterWrongAnswersToIntro(csvFilePath){
    console.log("---- writeNoFilterWrongAnswersToIntro, csvFilePath: ",csvFilePath)
    var fileStudyBit = csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4);            
    console.log("fileStudyBit: ",fileStudyBit);
    var baselinesIntro = {
        "Q11":"Down",
        "Q12": "From time 10 to 30 and time 60 to 70",
        "Q13":"Time 30",
        "Q17":"On the first long straight section",
        "Q14":"From a little before time 70 up to a little before time 80"
    }
    var objGenerated = [];
    const toFilterProlificIds = [];
    glbl_toFilterProlificIds = [];
    hashmap_toKeepProlificIds = {};
    var rArr = csv()
        .fromFile(csvFilePath)
        .then((objJson) => {
            console.log("loaded the file csvFilePath: ",csvFilePath)
            for (var k in objJson) {
                if (k >= 0) {
                    for (var key in objJson[k]) {
                        if (objJson[k][key] !== '') {
                            if (key == "Q11" || key == "Q12"  || key == "Q13" || key == "Q17" || key == "Q14")
                            {
                                if ( typeof(hashmap_toKeepProlificIds[objJson[k]["Q15"].toString()]) === "undefined" || hashmap_toKeepProlificIds[objJson[k]["Q15"].toString()]!=false )
                                {
                                    hashmap_toKeepProlificIds[objJson[k]["Q15"].toString()] = true;
                                }
                            }
                        }
                    }
                }
            }
            console.log("endOf then. toFilterProlificIds: ",toFilterProlificIds)

            csvFilePath = csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4);            
            var toFilter_JSONpath = "data/toFilter/notToFilter_"+csvFilePath+".json"
            // console.log("hashmap_toKeepProlificIds: ",hashmap_toKeepProlificIds,", toFilter_JSONpath: ", toFilter_JSONpath)
            fs.writeFile(toFilter_JSONpath, JSON.stringify(hashmap_toKeepProlificIds), function (err, data) {
                if (err) console.log('error', err);
            });

            return toFilterProlificIds     
        })
    // console.log("!!! toFilterProlificIds: ",toFilterProlificIds, ", glbl_toFilterProlificIds: ",glbl_toFilterProlificIds,", hashmap_toKeepProlificIds: ",hashmap_toKeepProlificIds,", rArr: ",rArr)
    return toFilterProlificIds;
}


function newGenerateModifiedCSV(QIDtoFilename, csvFilePath, fileHashmapToKeep="", addRandomInfoToFillVoid = false , fileEnd="") {
    // console.log("^^^^ newGenerateModifiedCSV, csvFilePath: ", csvFilePath,", fileEnd: ",fileEnd,", fileHashmapToKeep: ",fileHashmapToKeep);
    // console.log(QIDtoFilename);
    var objGenerated = [];
    var cptForBundlingQuestions = 0;
    var storeResponseId = "";
    var storeStartDate = "", storeEndDate = "", storeProgress = "", storeDuration_in_seconds = "", storeFinished = "", storeRecordedDate = "";

    var baseIndx_frstClck = 0;

    if (fileHashmapToKeep === "")
    {
        var strFileHashmapToKeep = "data/toFilter/toFilter_" + csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4) + fileEnd + ".json"
        // console.log("\n\nstrFileHashmapToKeep: ",strFileHashmapToKeep)
        fileHashmapToKeep = strFileHashmapToKeep
    }
    // console.log("**** trying to load csvFilePath: ",csvFilePath,", fileHashmapToKeep: ",fileHashmapToKeep);
    csv()
        .fromFile(csvFilePath)
        .then((objJson) => {
            // console.log("++++ loading done, csvFilePath: ",csvFilePath);
            // indx for values which are shared for all the participants. mod is for answers over several questions
            var indxStartDate = 1, indxEndDate = 2, indxStatus = 3, indxProgress = 5, indxDuration_seconds = 6, indxFinished = 7, indxRecordedDate = 8, indxResponseID = 9;
            var fileName = null, idc = null, drawnQn = null, drawnQl = null, queryString = null, flips = null, nMasks = null, dMask = null, dComplex_Qn = null, dComplex_Ql = null, dComplex_Where = null,
                focus = null, bslnA1 = null, bslnA2 = null, bslnA3 = null, bslnB = null, cntrQ = null;
            var modAnswerTime = null, modAnswerA1 = null, modAnswerA2 = null, modAnswerA3 = null, modAnswerB = null, modTrustA1 = null, modTrustA2 = null, modTrustA3 = null, modTrustB = null

            // console.log("QIDtoFilename: ", QIDtoFilename);
            // console.log({objJson});
            // load the names of files to filter
            var hashmapToKeep = fs.readFileSync(fileHashmapToKeep,'utf8')
            hashmapToKeep = JSON.parse(hashmapToKeep)

            var uniqueCntrQuestion = 0;

            for (var k in objJson) {
                // console.log("k: ", k,", objJson[k]: ",objJson[k]);
                // console.log("objJson[0]: ",objJson[0])
                if (k >= 0) {

                    // Verify if not an object to filter out!
                    var curProlificId = objJson[k]["Q15"];
                    // console.log('objJson[k]["Q15"]: ',objJson[k]["Q15"])
                    if (hashmapToKeep[curProlificId])
                    {
                        // Keep a boolean to indicate if the QID has a page submit...
                        var begAnswerIndx = null;
                        var firstAnswerGenerated = false;
                        var objShared = {};
                        // console.log("objJson[k][0]: ",objJson[k][0],', objJson[k]["StartDate"]: ', objJson[k]["StartDate"])
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
                                    // console.log("key: ", key, "(typeof key): ", (typeof key), ", objJson[k][key]: ", objJson[k][key], ", (typeof objJson[k][key]): ", (typeof objJson[k][key]), ", curIndx: ", curIndx)

                                    if (curIndx > 23) {
                                        var keyQual = "QID" + curIndx;
                                        // console.log("curIndx: ",curIndx,", QIDtoFilename: ",QIDtoFilename);
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
                                            objGenerated[objGenerated.length - 1]["dComplex_focus"] = (objGenerated[objGenerated.length - 1]["focus"]==="WHAT_Qn")?objGenerated[objGenerated.length - 1]["dComplex_Qn"] :  (objGenerated[objGenerated.length - 1]["focus"]==="WHAT_Ql")?objGenerated[objGenerated.length - 1]["dComplex_Ql"] : objGenerated[objGenerated.length - 1]["dComplex_Where"] 
                                            objGenerated[objGenerated.length - 1]["info_focus_dComplex_dMask"] = objGenerated[objGenerated.length - 1]["focus"]+"_"+objGenerated[objGenerated.length - 1]["dComplex_focus"]+"_"
                                                + ((objGenerated[objGenerated.length - 1]["dMask"] == "easy")?"E":(objGenerated[objGenerated.length - 1]["dMask"] == "medium")?"M":"H");

                                            objGenerated[objGenerated.length - 1]["info_dComplex_dMask"] = objGenerated[objGenerated.length - 1]["dComplex_focus"]+"_"
                                                + ((objGenerated[objGenerated.length - 1]["dMask"] == "easy")?"E":(objGenerated[objGenerated.length - 1]["dMask"] == "medium")?"M":"H");


                                            // console.log("Filled after first, ", objGenerated[objGenerated.length - 1])
                                        }
                                        if (key.indexOf("Submit") !== -1) {
                                            begAnswerIndx = curIndx;
                                            // console.log("Found Submit, objInfo: ", objInfo);
                                            objGenerated[objGenerated.length - 1]['t'] = objJson[k][key];
                                        } else {
                                            // console.log("begAnswerIndx: ",begAnswerIndx)
                                            if (curIndx > begAnswerIndx) {
                                                // console.log("objGenerated[objGenerated.length - 1]: ",objGenerated[objGenerated.length - 1]);
                                                (curIndx - begAnswerIndx === 1) ? objGenerated[objGenerated.length - 1]["answerA1"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 2) ? objGenerated[objGenerated.length - 1]["answerA2"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 3) ? objGenerated[objGenerated.length - 1]["answerA3"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 4) ? objGenerated[objGenerated.length - 1]["answerB"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 5) ? objGenerated[objGenerated.length - 1]["trustA1"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 6) ? objGenerated[objGenerated.length - 1]["trustA2"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 7) ? objGenerated[objGenerated.length - 1]["trustA3"] = objJson[k][key] :
                                                objGenerated[objGenerated.length - 1]["trustB"] = objJson[k][key]
                                                if(curIndx-begAnswerIndx === 7 )
                                                {
                                                    objGenerated[objGenerated.length-1]["uniqueCntrQuestion"] = uniqueCntrQuestion;
                                                    uniqueCntrQuestion++;
                                                }
                                            }
                                        }
                                    }
                                    if (typeof objGenerated[objGenerated.length - 1] !== "undefined")
                                    {
                                        // console.log("adding differences; objGenerated[objGenerated.length - 1]: ",objGenerated[objGenerated.length - 1])
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
                                            console.log("numDerivedB: ",numDerivedB, ', objGenerated[objGenerated.length - 1]["bslnB"]: ',objGenerated[objGenerated.length - 1]["bslnB"], ', calculation: ', (1 * objGenerated[objGenerated.length - 1]["bslnB"]==numDerivedB) )
                                            objGenerated[objGenerated.length - 1]["correctB"] = 1 * (objGenerated[objGenerated.length - 1]["bslnB"]==numDerivedB);
                                        }
                                        // if(strAttr!==null)objGenerated[objGenerated.length-1][strAttr]=objJson[k][key];
                                    }
                                }
                            }

                        }
                    } else {
                        console.log("ignoring the answer from the prolific id: ", curProlificId)
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

            var fileStudyBit = csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4);            
            var t = new Date(); var fileNameTime = (addRandomInfoToFillVoid) ? "randomlyfilled_" + t.getTime() : fileStudyBit;
            console.log("\n==== writing " + "data/transformed/survey_" + fileNameTime + fileEnd + ".csv")
            fs.writeFile("data/transformed/survey_" + fileNameTime + fileEnd + ".csv", csvOutput, function (err, data) {
                if (err) console.log('error', err);
            });
        })
}

function newNoFilterGenerateModifiedCSV(QIDtoFilename, csvFilePath, fileHashmapToKeep="", addRandomInfoToFillVoid = false) {
    // console.log("newGenerateModifiedCSV ",{QIDtoFilename,csvFilePath,addRandomInfoToFillVoid})
    // console.log(QIDtoFilename);
    var objGenerated = [];
    var cptForBundlingQuestions = 0;
    var storeResponseId = "";
    var storeStartDate = "", storeEndDate = "", storeProgress = "", storeDuration_in_seconds = "", storeFinished = "", storeRecordedDate = "";

    var baseIndx_frstClck = 0;

    if (fileHashmapToKeep === "")
    {
        fileHashmapToKeep = "data/toFilter/notToFilter_" + csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4) + ".json"
    }
    console.log("trying to load csvFilePath: ",csvFilePath,", fileHashmapToKeep: ",fileHashmapToKeep);
    csv()
        .fromFile(csvFilePath)
        .then((objJson) => {
            console.log("loading done, csvFilePath: ",csvFilePath);
            // indx for values which are shared for all the participants. mod is for answers over several questions
            var indxStartDate = 1, indxEndDate = 2, indxStatus = 3, indxProgress = 5, indxDuration_seconds = 6, indxFinished = 7, indxRecordedDate = 8, indxResponseID = 9;
            var fileName = null, idc = null, drawnQn = null, drawnQl = null, queryString = null, flips = null, nMasks = null, dMask = null, dComplex_Qn = null, dComplex_Ql = null, dComplex_Where = null,
                focus = null, bslnA1 = null, bslnA2 = null, bslnA3 = null, bslnB = null, cntrQ = null;
            var modAnswerTime = null, modAnswerA1 = null, modAnswerA2 = null, modAnswerA3 = null, modAnswerB = null, modTrustA1 = null, modTrustA2 = null, modTrustA3 = null, modTrustB = null

            // console.log("QIDtoFilename: ", QIDtoFilename);
            // console.log({objJson});

            // load the names of files to filter
            var hashmapToKeep = fs.readFileSync(fileHashmapToKeep,'utf8')
            hashmapToKeep = JSON.parse(hashmapToKeep)
            console.log("##hashmapToKeep: ",hashmapToKeep)
            console.log('hashmapToKeep["612ab102d4cefe24f5a0775b"]: ',hashmapToKeep["612ab102d4cefe24f5a0775b"])

            var uniqueCntrQuestion = 0;

            for (var k in objJson) {
                // console.log("k: ", k,", objJson[k]: ",objJson[k]);
                // console.log("objJson[0]: ",objJson[0])
                if (k >= 0) {

                    // Verify if not an object to filter out!
                    var curProlificId = objJson[k]["Q15"];
                    // console.log('objJson[k]["Q15"]: ',objJson[k]["Q15"])
                    if (hashmapToKeep[curProlificId])
                    {
                        // Keep a boolean to indicate if the QID has a page submit...
                        var begAnswerIndx = null;
                        var firstAnswerGenerated = false;
                        var objShared = {};
                        // console.log("objJson[k][0]: ",objJson[k][0],', objJson[k]["StartDate"]: ', objJson[k]["StartDate"])
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
                                    // console.log("key: ", key, "(typeof key): ", (typeof key), ", objJson[k][key]: ", objJson[k][key], ", (typeof objJson[k][key]): ", (typeof objJson[k][key]), ", curIndx: ", curIndx)

                                    // prolific id
                                    if (curIndx == 15){

                                    } 
                                    else if (curIndx == 11){
                                        // Question for trajectory going down

                                    } 
                                    else if(curIndx == 12){
                                        // Find when the qualitative attribute is on

                                    } 
                                    else if (curIndx == 13){
                                        // find time with minimum

                                    } 
                                    else if (curIndx == 17){
                                        // find position with minimum

                                    } 
                                    else if (curIndx == 14){
                                        // find when the mask is on
                                        
                                    }

                                    if (curIndx > 23) {
                                        var keyQual = "QID" + curIndx;
                                        // console.log("curIndx: ",curIndx,", QIDtoFilename: ",QIDtoFilename);
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
                                            objGenerated[objGenerated.length - 1]["dComplex_focus"] = (objGenerated[objGenerated.length - 1]["focus"]==="WHAT_Qn")?objGenerated[objGenerated.length - 1]["dComplex_Qn"] :  (objGenerated[objGenerated.length - 1]["focus"]==="WHAT_Ql")?objGenerated[objGenerated.length - 1]["dComplex_Ql"] : objGenerated[objGenerated.length - 1]["dComplex_Where"] 
                                            objGenerated[objGenerated.length - 1]["info_focus_dComplex_dMask"] = objGenerated[objGenerated.length - 1]["focus"]+"_"+objGenerated[objGenerated.length - 1]["dComplex_focus"]+"_"
                                                + ((objGenerated[objGenerated.length - 1]["dMask"] == "easy")?"E":(objGenerated[objGenerated.length - 1]["dMask"] == "medium")?"M":"H");

                                            objGenerated[objGenerated.length - 1]["info_dComplex_dMask"] = objGenerated[objGenerated.length - 1]["dComplex_focus"]+"_"
                                                + ((objGenerated[objGenerated.length - 1]["dMask"] == "easy")?"E":(objGenerated[objGenerated.length - 1]["dMask"] == "medium")?"M":"H");


                                            // console.log("Filled after first, ", objGenerated[objGenerated.length - 1])
                                        }
                                        if (key.indexOf("Submit") !== -1) {
                                            begAnswerIndx = curIndx;
                                            // console.log("Found Submit, objInfo: ", objInfo);
                                            objGenerated[objGenerated.length - 1]['t'] = objJson[k][key];
                                        } else {
                                            // console.log("begAnswerIndx: ",begAnswerIndx)
                                            if (curIndx > begAnswerIndx) {
                                                // console.log("objGenerated[objGenerated.length - 1]: ",objGenerated[objGenerated.length - 1]);
                                                (curIndx - begAnswerIndx === 1) ? objGenerated[objGenerated.length - 1]["answerA1"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 2) ? objGenerated[objGenerated.length - 1]["answerA2"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 3) ? objGenerated[objGenerated.length - 1]["answerA3"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 4) ? objGenerated[objGenerated.length - 1]["answerB"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 5) ? objGenerated[objGenerated.length - 1]["trustA1"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 6) ? objGenerated[objGenerated.length - 1]["trustA2"] = objJson[k][key] :
                                                (curIndx - begAnswerIndx === 7) ? objGenerated[objGenerated.length - 1]["trustA3"] = objJson[k][key] :
                                                objGenerated[objGenerated.length - 1]["trustB"] = objJson[k][key]
                                                if(curIndx-begAnswerIndx === 7 )
                                                {
                                                    objGenerated[objGenerated.length-1]["uniqueCntrQuestion"] = uniqueCntrQuestion;
                                                    uniqueCntrQuestion++;
                                                }
                                            }
                                        }
                                    }
                                    if (typeof objGenerated[objGenerated.length - 1] !== "undefined")
                                    {
                                        // console.log("adding differences; objGenerated[objGenerated.length - 1]: ",objGenerated[objGenerated.length - 1])
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
                                            console.log("numDerivedB: ",numDerivedB, ', objGenerated[objGenerated.length - 1]["bslnB"]: ',objGenerated[objGenerated.length - 1]["bslnB"], ', calculation: ', (1 * objGenerated[objGenerated.length - 1]["bslnB"]==numDerivedB) )
                                            objGenerated[objGenerated.length - 1]["correctB"] = 1 * (objGenerated[objGenerated.length - 1]["bslnB"]==numDerivedB);
                                        }
                                        // if(strAttr!==null)objGenerated[objGenerated.length-1][strAttr]=objJson[k][key];
                                    }
                                }
                            }

                        }
                    } else {
                        console.log("ignoring the answer from the prolific id: ", curProlificId)
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

            var fileStudyBit = csvFilePath.split('/')[csvFilePath.split('/').length-1].substring(0,csvFilePath.split('/')[csvFilePath.split('/').length-1].length-4);            
            var t = new Date(); var fileNameTime = (addRandomInfoToFillVoid) ? "randomlyfilled_" + t.getTime() : fileStudyBit;
            console.log("writing " + "data/transformed/survey_" + fileNameTime  + ".csv")
            fs.writeFile("data/transformed/survey_" + fileNameTime + ".csv", csvOutput, function (err, data) {
                if (err) console.log('error', err);
            });
        })
}


// New function: we need to establish the distribution of baselines.
function generateBaselineCSV(QIDtoFilename,additionNameBaseline="") {
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
            focusDiff = (focusDiff === "E") ? "Easy" : (focusDiff === "M") ? "Medium" : "Hard"

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
    console.log("writing : " + "data/transformed/baselines/baselines_"+additionNameBaseline+".csv")
    fs.writeFile("data/transformed/baselines/baselines_"+additionNameBaseline+".csv", csvOutputN, function (err, data) {
        if (err) console.log('error', err);
    });
}


// ---- Calls of the functions 
// generateModifiedCSV(QIDtoFilename,csvFilePath)


// Commented for tests, but should work
/* Write the functions in separate calls. Something much cleanier could be done but we are in a rush */
// let rawdata_cmplt_f = fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_complete_measurement_f_2021_09_18.json");
// let rawdata_cmplt_nf = fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_complete_measurement_nf_2021_09_18.json");
// let rawdata_dist_h =fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_complete_distractor_h_2021_09_18.json");
// let rawdata_dist_n =fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_complete_distractor_n_2021_09_18.json");
// let rawdata_scaling_0 = fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_complete_scaling_0_2021_09_19.json");
// let rawdata_scaling_1 = fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_complete_scaling_1_2021_09_19.json");
// let rawdata_scaling_2 = fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_complete_scaling_2_2021_09_19.json");
// let rawdata_dist_h =fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_addendum_MMM_distractor_h_replaced_mix.json");
// let rawdata_dist_n =fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_addendum_MMM_distractor_n_replaced_mix.json");
// let rawdata_scaling_0 = fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_addendum_MMM_scaling_0_replaced_mix.json");
// let rawdata_scaling_1 = fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_addendum_MMM_scaling_1_replaced_mix.json");
// let rawdata_scaling_2 = fs.readFileSync("data/studies_2021_09_18/QIDtoFilename_addendum_MMM_scaling_2_replaced_mix.json");
// -- Update for First Language English (first batch)
// let rawdata_cmplt_f_firstLanguage = fs.readFileSync("data/studies_2021_10_22/QIDtoFilename_complete_measurement_f_2021_09_18_October_22_headerAdapted.json")
// let rawdata_cmplt_nf_firstLanguage = fs.readFileSync("data/studies_2021_10_22/QIDtoFilename_complete_measurement_nf_2021_09_18_October_22_headerAdapted.json")
// -- Update for First Language English (no issue with stimuli seen with the cntrQ)
// let rawdata_full_distractor_h = fs.readFileSync("data/studies_2021_11_03/QIDtoFilename_full_distractor_h.json");
// let rawdata_full_distractor_n = fs.readFileSync("data/studies_2021_11_03/QIDtoFilename_full_distractor_n.json");
// let rawdata_full_scaling_0 = fs.readFileSync("data/studies_2021_11_03/QIDtoFilename_full_scaling_0.json");
// let rawdata_full_scaling_1 = fs.readFileSync("data/studies_2021_11_03/QIDtoFilename_full_scaling_1.json");
// let rawdata_full_scaling_2 = fs.readFileSync("data/studies_2021_11_03/QIDtoFilename_full_scaling_2.json");

// -- noTikTok
let data_measurement_nf = fs.readFileSync("Studies_2021_11_12/measurement_nf/QIDtoFilename_measurement_nf.json")
let data_measurement_f = fs.readFileSync("Studies_2021_11_12/measurement_f/QIDtoFilename_measurement_f.json")
let data_distractor_h = fs.readFileSync("Studies_2021_11_12/distractor_h/QIDtoFilename_distractor_h.json")
let data_distractor_n = fs.readFileSync("Studies_2021_11_12/distractor_n/QIDtoFilename_distractor_n.json")
let data_scaling_0 = fs.readFileSync("Studies_2021_11_12/scaling_0/QIDtoFilename_scaling_0.json")
let data_scaling_1 = fs.readFileSync("Studies_2021_11_12/scaling_1/QIDtoFilename_scaling_1.json")
let data_scaling_2 = fs.readFileSync("Studies_2021_11_12/scaling_2/QIDtoFilename_scaling_2.json")

// Fluent only
// let d_measurement_nf = fs.readFileSync('data/QIDtoFilename_complete_measurement_nf_2021_09_18.json')
// var listQIDtoFileName = []; 
// listQIDtoFileName.push(JSON.parse(rawdata_cmplt_nf)); 
// listQIDtoFileName.push(JSON.parse(rawdata_cmplt_f));
// listQIDtoFileName.push(JSON.parse(rawdata_dist_h)); 
// listQIDtoFileName.push(JSON.parse(rawdata_dist_n));
// listQIDtoFileName.push(JSON.parse(rawdata_scaling_0)); 
// listQIDtoFileName.push(JSON.parse(rawdata_scaling_1));
// listQIDtoFileName.push(JSON.parse(rawdata_scaling_2)); 
// -- Update for First Language English
// var listQIDtoFileName_firstLanguage = []; 
// listQIDtoFileName_firstLanguage.push(JSON.parse(rawdata_cmplt_f_firstLanguage)); 
// listQIDtoFileName_firstLanguage.push(JSON.parse(rawdata_cmplt_nf_firstLanguage));
// listQIDtoFileName_firstLanguage.push(JSON.parse(rawdata_full_distractor_h)); 
// listQIDtoFileName_firstLanguage.push(JSON.parse(rawdata_full_distractor_n));
// listQIDtoFileName_firstLanguage.push(JSON.parse(rawdata_full_scaling_0)); 
// listQIDtoFileName_firstLanguage.push(JSON.parse(rawdata_full_scaling_1));
// listQIDtoFileName_firstLanguage.push(JSON.parse(rawdata_full_scaling_2));
// Fluent only
// var listStrFilesBsln = [];
// listStrFilesBsln.push("cmplt_nf");
// listStrFilesBsln.push("cmplt_f");
// listStrFilesBsln.push("dist_h");
// listStrFilesBsln.push("dist_n");
// listStrFilesBsln.push("scaling_0");
// listStrFilesBsln.push("scaling_1");
// listStrFilesBsln.push("scaling_2");
// First language and fluent
// var listStrFilesBsln_firstLanguage = [];
// listStrFilesBsln_firstLanguage.push("cmplt_nf_firstLanguage");
// listStrFilesBsln_firstLanguage.push("cmplt_f_firstLanguage");
// listStrFilesBsln_firstLanguage.push("full_distractor_h_firstLanguage");
// listStrFilesBsln_firstLanguage.push("full_distractor_n_firstLanguage");
// listStrFilesBsln_firstLanguage.push("full_scaling_0_firstLanguage");
// listStrFilesBsln_firstLanguage.push("full_scaling_1_firstLanguage");
// listStrFilesBsln_firstLanguage.push("full_scaling_2_firstLanguage");

var listQIDtoFileName_noTikTok = []; 
listQIDtoFileName_noTikTok.push(JSON.parse(data_measurement_nf)); 
listQIDtoFileName_noTikTok.push(JSON.parse(data_measurement_f));
listQIDtoFileName_noTikTok.push(JSON.parse(data_distractor_h)); 
listQIDtoFileName_noTikTok.push(JSON.parse(data_distractor_n));
listQIDtoFileName_noTikTok.push(JSON.parse(data_scaling_0)); 
listQIDtoFileName_noTikTok.push(JSON.parse(data_scaling_1));
listQIDtoFileName_noTikTok.push(JSON.parse(data_scaling_2)); 

var listStrFilesBsln_noTikTok = [];
listStrFilesBsln_noTikTok.push("noTikTok_measurement_nf");
listStrFilesBsln_noTikTok.push("noTikTok_measurement_f");
listStrFilesBsln_noTikTok.push("noTikTok_distractor_h");
listStrFilesBsln_noTikTok.push("noTikTok_distractor_n");
listStrFilesBsln_noTikTok.push("noTikTok_scaling_0");
listStrFilesBsln_noTikTok.push("noTikTok_scaling_1");
listStrFilesBsln_noTikTok.push("noTikTok_scaling_2");

/** Function calls to generate baseline files. Uncomment selections relevant. 
 * The code requires to be run three times, uncommenting each section which generates a file first. Could be made better but we are in a rush.
 *
// ======== Fluent speakers
// ---- Generate baselines -- fluent
// var t = new Date(); // var strAdd_bslnFile = "_measurement_nf_"+t.getFullYear()+"_"+t.getMonth()+"_"+t.getDay()+"_"+t.getHours()+"_"+t.getMinutes()
// console.log("---- Generate baselines")
// for (var i=0; i < listQIDtoFileName.length; i++)
// {
//     generateBaselineCSV(listQIDtoFileName[i],listStrFilesBsln[i]);  
// }
// ---- Filter out answers with wrong intro
// console.log("---- Filter out answers with wrong intro")
// for (var i=0; i < csvFilePathArray_fluent.length;i++){
//     writeFilterWrongAnswersToIntro(csvFilePathArray_fluent[i]);
// }
// // // ---- Generate structured files of answers
// console.log("---- Generate structured files of answers, csvFilePathArray_fluent: ",csvFilePathArray_fluent)
// for (var i=0; i < listQIDtoFileName.length; i++){
//     console.log("csvFilePathArray_fluent[i]: ",csvFilePathArray_fluent[i]);
//     newGenerateModifiedCSV(listQIDtoFileName[i], csvFilePathArray_fluent[i] , false, "_firstLanguage")
// }
// ======== First language and fluent
// // ---- Generate baselines -- fluent & first language // still ok on 2021_11_03
// console.log("---- Generate baselines")
// for (var i=0; i < listQIDtoFileName_firstLanguage.length; i++)
// {
//     generateBaselineCSV(listQIDtoFileName_firstLanguage[i],listStrFilesBsln_firstLanguage[i]);  
// }
// // // ---- Filter out answers with wrong intro // seems ok on 2021_11_03
// console.log("---- Filter out answers with wrong intro. UPDATE: hopefully not")
// for (var i=0; i < csvFilePathArray_FirstLanguage.length;i++){
//     // writeFilterWrongAnswersToIntro(csvFilePathArray_FirstLanguage[i]);
//     writeNoFilterWrongAnswersToIntro(csvFilePathArray_FirstLanguage[i]);
// }
// // ---- Generate structured files of answers
// console.log("---- Generate structured files of answers, csvFilePathArray_FirstLanguage: ",csvFilePathArray_FirstLanguage)
// for (var i=0; i < listQIDtoFileName_firstLanguage.length; i++){
//     console.log("csvFilePathArray_FirstLanguage[i]: ",csvFilePathArray_FirstLanguage[i]);
//     // newGenerateModifiedCSV(listQIDtoFileName_firstLanguage[i], csvFilePathArray_FirstLanguage[i])
//     newNoFilterGenerateModifiedCSV(listQIDtoFileName_firstLanguage[i], csvFilePathArray_FirstLanguage[i], false, "_firstLanguage")
// }
*/

// ======== Prior to TikTok trend
// ---- Generate baselines -- fluent & first language // still ok on 2021_11_03
// console.log("---- Generate baselines")
// for (var i=0; i < listQIDtoFileName_noTikTok.length; i++)
// {
//     generateBaselineCSV(listQIDtoFileName_noTikTok[i],listStrFilesBsln_noTikTok[i]);  
// }

// // ---- Filter out answers with wrong intro // seems ok on 2021_11_03
// console.log("---- Filter out answers with wrong intro.")
// for (var i=0; i < csvFilePathArray_noTikTok.length;i++){
//     // writeNoFilterWrongAnswersToIntro(csvFilePathArray_noTikTok[i]); // to use if we wish outputs without filters
//     writeFilterWrongAnswersToIntro(csvFilePathArray_noTikTok[i], "_noTikTok")
// }

// // ---- Generate structured files of answers
console.log("---- Generate structured files of answers, csvFilePathArray_noTikTok: ",csvFilePathArray_noTikTok)
for (var i=0; i < listQIDtoFileName_noTikTok.length; i++){
    console.log("csvFilePathArray_noTikTok[i]: ",csvFilePathArray_noTikTok[i]);
    // newNoFilterGenerateModifiedCSV(listQIDtoFileName_noTikTok[i], csvFilePathArray_noTikTok[i])
    newGenerateModifiedCSV (listQIDtoFileName_noTikTok[i], csvFilePathArray_noTikTok[i], "" , false, "_noTikTok" )
}
