console.log("---- dataTransform.js")
// https://paper.dropbox.com/doc/Notes-about-coding-for-results-analysis--BIcFyz4C8sfOP9sNxjJhTANNAQ-2QtCwBvX4Vng9cWFJjlds
var $ = require( "jquery" );
const csvparser = require('csv-parser');
const fs = require('fs');
// I'm going to transform the csv into json, modify it accordingly, and then create a new csv.
const csv=require('csvtojson')
console.log(Math.random())
// ---- Data sources & global variables
// const csvFilePath='data/survey_precise-study_20210410_1500_April 10, 2021_07.36.csv'
const csvFilePath='data/survey_precise-study_20210410_1500_April 10, 2021_07.36_modified.csv'
// $.getJSON("QIDtoFilename.json", function(json) {
let rawdata = fs.readFileSync('data/QIDtoFilename.json');
let QIDtoFilename = JSON.parse(rawdata);
hashmapAttributesNames = {
    "eG": "engine temperature",
    "eT":"engine temperature", // there's a bug in the data generation, but not a big issue
    "fC":"fuel consumption",
    "eC": "electricity consumption",
    "sF": "suspension force",
    "gO": "gps on",
    "wO": "wiper on",
    "rO": "radio on",
    "hO": "heating on"    
}

// ---- Function definitions

function generateModifiedCSV(QIDtoFilename,csvFilePath,addRandomInfoToFillVoid=false){

    // console.log(QIDtoFilename);
    var objGenerated=[];
    var cptForBundlingQuestions=0;
    var storeResponseId="";
    var storeStartDate="",storeEndDate="",storeProgress="",storeDuration_in_seconds="",storeFinished="",storeRecordedDate="";

    csv()
    .fromFile(csvFilePath)
    .then((jsonObj)=>{
        for(var k in jsonObj){
            console.log("k: ",k);
            for(var key in jsonObj[k]){
                // console.log("k: ",k,", key: ",key,", jsonObj[k][key]: ", jsonObj[k][key],', key.indexOf("_First Click")!==-1: ',key.indexOf("_First Click")!==-1);
                
                if (key==="ResponseId"){storeResponseId=jsonObj[k][key]}
                if(key==="Progress"){storeProgress=jsonObj[k][key]}
                if(key==="StartDate"){storeStartDate=jsonObj[k][key]} if(key==="EndDate"){storeEndDate=jsonObj[k][key]}
                if(key==="Duration (in seconds)"){storeDuration_in_seconds=jsonObj[k][key]}if(key==="Finished"){storeFinished=jsonObj[k][key]}
                if(key==="RecordedDate"){storeRecordedDate=jsonObj[k][key]}


                var numID = (key.indexOf('_')!==-1)?Number(key.substr(1,key.indexOf('_')-1)):Number(key.substr(1));
                // console.log("numID",numID,", numID%5",(numID%5))

                if (key.indexOf("_First Click")!==-1){  // New line for a question
                    objGenerated.push({})
                    objGenerated[objGenerated.length-1]["ResponseId"]=storeResponseId;
                    objGenerated[objGenerated.length-1]["Progress"]=storeProgress; objGenerated[objGenerated.length-1]["RecordedDate"]=storeRecordedDate;
                    objGenerated[objGenerated.length-1]["StartDate"]=storeStartDate; objGenerated[objGenerated.length-1]["EndDate"]=storeEndDate;
                    objGenerated[objGenerated.length-1]["Finished"]=storeFinished;objGenerated[objGenerated.length-1]["Duration_in_seconds"]=storeDuration_in_seconds; 

                    var keyQual = "QID"+numID;
                    objGenerated[objGenerated.length-1]["filename"]=QIDtoFilename[keyQual]; 
                    var filename = QIDtoFilename[keyQual]
                    var objInfo = extractColumnsFromFilename(filename);
                    for(var infoK in objInfo){
                        objGenerated[objGenerated.length-1][infoK] = objInfo[infoK];
                    }
                    console.log("k: ",k,", key: ",key,", jsonObj[k][key]: ", jsonObj[k][key],', key.indexOf("_First Click")!==-1: ',key.indexOf("_First Click")!==-1,', objGenerated[objGenerated.length-1]["cntrQ"]: ',objGenerated[objGenerated.length-1]["cntrQ"]);

                    (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? objGenerated[objGenerated.length-1]["FirstClick"]=Math.random()*20
                    : objGenerated[objGenerated.length-1]["FirstClick"]=jsonObj[k][key];

                } else if (key.indexOf("_Last Click")!==-1){
                    (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? objGenerated[objGenerated.length-1]["LastClick"] = objGenerated[objGenerated.length-1]["FirstClick"]+Math.random()*10
                    : objGenerated[objGenerated.length-1]["LastClick"]=jsonObj[k][key];
                } else if (key.indexOf("_Page Submit")!==-1){
                    (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? objGenerated[objGenerated.length-1]["PageSubmit"] = objGenerated[objGenerated.length-1]["LastClick"]+Math.random()*15
                    : objGenerated[objGenerated.length-1]["PageSubmit"]=jsonObj[k][key];
                } else if ( numID>15 && numID%5==2 ){
                    // Adapt for matching with the baseline as numbers: 1,0 and -1 according to agree, neither and disagree
                    // Neither agree nor disagree // Disagree // Agree
                    var rndNumForGen = Math.random();
                    console.log(rndNumForGen)
                    console.log("jsonObj[k][key]==='' && addRandomInfoToFillVoid: ",jsonObj[k][key]==='' && addRandomInfoToFillVoid);
                    objGenerated[objGenerated.length-1]["QuestionA"] = (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? ((rndNumForGen<0.33)?-1:(rndNumForGen<0.66)?0:1)
                    : ((jsonObj[k][key].indexOf("Neither agree nor disagree")!==-1)?0:(jsonObj[k][key].indexOf("Disagree")!==-1)?-1:(jsonObj[k][key].indexOf("Agree")!==-1)?1:'');

                    // Add if answer is correct! 
                    objGenerated[objGenerated.length-1]["correctA"] = 1*(objGenerated[objGenerated.length-1]["QuestionA"]===objGenerated[objGenerated.length-1]["bslnA"])
                }else if ( numID>15 && numID%5==3 ){
                    var rndNumForGen = Math.random()
                    objGenerated[objGenerated.length-1]["TrustA"] = (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? (Math.floor(Math.random()*6))
                    : jsonObj[k][key];
                }else if ( numID>15 && numID%5==4 ){
                    // Adapt for matching with the baseline as numbers: 1,0 and -1 according to agree, neither and disagree
                    var rndNumForGen = Math.random()
                    objGenerated[objGenerated.length-1]["QuestionB"] = (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? ((rndNumForGen<0.33)?-1:(rndNumForGen<0.66)?0:1)
                    : ((jsonObj[k][key].indexOf("Neither agree nor disagree")!==-1)?0:(jsonObj[k][key].indexOf("Disagree")!==-1)?-1:(jsonObj[k][key].indexOf("Agree")!==-1)?1:'');
                } else if ( numID>15 && numID%5==0 ){
                    var rndNumForGen = Math.random()
                    objGenerated[objGenerated.length-1]["TrustB"] = (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? (Math.floor(Math.random()*6))
                    : jsonObj[k][key];

                    // Add if answer is correct!
                    objGenerated[objGenerated.length-1]["correctB"] = 1*(objGenerated[objGenerated.length-1]["QuestionB"]===objGenerated[objGenerated.length-1]["bslnB"])
                }
                
            }
        }

        // const items = jsonObj;
        const items = objGenerated
        const replacer = (key, value) => value === null ? '' : value // specify how you want to handle null values here
        const header = Object.keys(items[0])
        console.log("header: ",header);
        const csvOutput = [
        header.join(','), // header row first
        ...items.map(row => header.map(fieldName => JSON.stringify(row[fieldName], replacer)).join(','))
        ].join('\r\n')
        
        // console.log(csvOutput)
        var t = new Date(); var fileNameTime=(addRandomInfoToFillVoid)? "randomlyfilled_"+t.getTime() : t.getTime();
        fs.writeFile("data/transformed/survey_precise-study_"+fileNameTime+".csv", csvOutput, function (err, data) {
            if(err) console.log('error', err);
        });

    })
}

// e.g. idc39-eG-wO-qry_fC_gte_98.05-flips_h0v0-nMasks_2-dMask_easy-dCmplx_EHE-fcs_WHAT_Qn-bsln_t_t-cntrQ_251
function extractColumnsFromFilename(filename){
    var idc=Number(filename.substr("idc".length, filename.indexOf('-') - "idc".length));
    var splitFN = filename.split('-');
    var drawnQn=hashmapAttributesNames[splitFN[1]];
    var drawnQl=hashmapAttributesNames[splitFN[2]];
    var queryArr=splitFN[3].split('_');
    var queryString= hashmapAttributesNames[queryArr[1]]+" "+queryArr[2]+" "+queryArr[3]
    var flips = splitFN[4].substr(splitFN[4].indexOf('_')+1)
    var nMasks = Number(splitFN[5].substr(splitFN[5].indexOf('_')+1))
    var dMask = splitFN[6].substr(splitFN[6].indexOf('_')+1)
    var dComplex = splitFN[7].substr(splitFN[7].indexOf('_')+1); var dComplex_Qn = dComplex[0], dComplex_Ql = dComplex[1], dComplex_Where = dComplex[2];
    var focus = splitFN[8].substr(splitFN[8].indexOf('_')+1);
    var bslnA = (splitFN[9].substr(splitFN[9].indexOf('_')+1,1) === 't' )?1:-1, bslnB= (splitFN[9].substr(splitFN[9].length-1) === 't')?1:-1;
    var cntrQ = Number(splitFN[10].substr(splitFN[10].indexOf('_')+1));

    return {
        "idc":idc,"drawnQn":drawnQn,"drawnQl":drawnQl,"queryString":queryString,"flips":flips,"nMasks":nMasks,"dMask":dMask,"dComplex_Qn":dComplex_Qn,"dComplex_Ql":dComplex_Ql,"dComplex_Where":dComplex_Where,"focus":focus,"bslnA":bslnA,"bslnB":bslnB,"cntrQ":cntrQ
    }
}


// ---- Calls of the functions 
// generateModifiedCSV(QIDtoFilename,csvFilePath)
generateModifiedCSV(QIDtoFilename,csvFilePath,true)