console.log("---- merge objects stimuli and responses")

var $ = require("jquery");
const csvparser = require("csv-parser");
const fs = require("fs");
const csv = require("csvtojson");
const { FORMERR } = require("dns");

console.log("loaded bases")

var d = new Date();
var strTime = d.getFullYear()+"_"+(d.getMonth()+1)+"_"+d.getUTCDate()+"_"+d.getHours();
var fileAccordingToTime = "noFilter_survey_measurement_all_headerAdapted_flipWritten_"+strTime;

// var objResponses = 
//     fs.readFileSync("Data_2022_05_12/Outputs/noFilter_survey_measurement_all_headerAdapted_flipWritten_2022_5_13_17.json");
var objResponses = 
    fs.readFileSync("Data_2022_05_12/Outputs/"+fileAccordingToTime+".json");

objResponses = JSON.parse(objResponses);

var stimuli = 
    fs.readFileSync("Data_2022_05_12/Sources/allObjects_1-486_2022-05-11.json");
stimuli = JSON.parse(stimuli);

console.log("loaded the files")
var firstObject = objResponses[0];

for(var s in stimuli){
    console.log("current cntrQ: ",stimuli[s]["cntrQ"]);


    // Add the stability comparison matching the bslnB
    stimuli[s].stabilityComparison = stimuli[s].numDiffSC<0;
    stimuli[s].issueStabilityComparison = 
        (stimuli[s].stablityComparison>0) != stimuli[s].bslnB;

    for(var key in firstObject){
        if(stimuli[s][key] === undefined){
            stimuli[s][key] = [];
            var respMatchCntrQ = objResponses.filter(a => a.cntrQ === stimuli[s]["cntrQ"])

            for(var i in respMatchCntrQ){
                if (key === "Finished" 
                || key === "impossibleQualAnswer" 
                || key === "stimuliAllTrust0or5" 
                || key ==="responseAllTrust0or5" 
                || key ==="passedIntroQuestions"){
                    respMatchCntrQ[i][key] = Boolean(respMatchCntrQ[i][key])
                    if(respMatchCntrQ[i]["focus"]!=="WHAT_Ql"){
                        respMatchCntrQ[i]["impossibleQualAnswer"] = undefined;
                    }
                }
                
                if( typeof( respMatchCntrQ[i]["prolificId"] ) === "number" ){
                    respMatchCntrQ[i]["prolificId"] = undefined
                }
                // .toString().indexOf("artifProlif") !== -1 ){ respMatchCntrQ[i]["prolificId"] = undefined; }

                stimuli[s][key].push( respMatchCntrQ[i][key] );



            }
        }
    }
}

fs.writeFile("Data_2022_05_12/Outputs/stimuli_enriched_"+strTime+".json", JSON.stringify(stimuli), function (err, data) { if (err) console.log('error', err); });            
