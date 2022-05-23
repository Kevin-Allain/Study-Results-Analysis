console.log("---- Differences results Kevin and Jason")
var $ = require("jquery");
const csvparser = require("csv-parser");
const fs = require("fs");
const csv = require("csvtojson");

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

Array.prototype.indicesOf = function(query) {
    var indices = [];
    var x = this.indexOf(query);
    while(x != -1) {
        indices.push(x);
        x = this.indexOf(query, x + 1);
    }
    return indices;
};


// fileKevin 
var strFileKevin = "Data_2022_05_12/Outputs/stimuli_enriched_2022_5_16_21.json"
var fKevin = fs.readFileSync( strFileKevin );
fKevin = JSON.parse(fKevin);

var strFileJason = "Jason_Produced_Data/dataMap.json"
var fJason = fs.readFileSync( strFileJason );
fJason = JSON.parse(fJason);

var cptJ_Ql=0;

var cptDiffA1_WHAT_Ql = 0,cptDiffA2_WHAT_Ql = 0,cptDiffA3_WHAT_Ql = 0, cptDiffB_WHAT_Ql=0;
var cptDiffA1_WHAT_Qn = 0,cptDiffA2_WHAT_Qn = 0,cptDiffA3_WHAT_Qn = 0, cptDiffB_WHAT_Qn=0;
var cptDiffA1_WHERE = 0,cptDiffA2_WHERE = 0,cptDiffA3_WHERE = 0, cptDiffB_WHERE=0;

for(var j=0; j<fJason.length; j++ ){
    // let's look at the first object
    // if(cptJ_Ql===0 && fJason[j][1].focus==="WHAT_Ql"){
    //     console.log(fJason[j][1]);
    //     cptJ_Ql++;
    // }

    console.log("j: ",j);

    var curFocus = fJason[j][1].focus;

    var sameBslnA1 = fJason[j][1]["jdBaselines"].bslnA1check === fJason[j][1]["kaBaselines"].bslnA1
    var sameBslnA2 = fJason[j][1]["jdBaselines"].bslnA2check === fJason[j][1]["kaBaselines"].bslnA2
    var sameBslnA3 = fJason[j][1]["jdBaselines"].bslnA3check === fJason[j][1]["kaBaselines"].bslnA3

    // no trace of JD baseline B when focus is WHAT_Ql or WHERE?
    // var sameBslnB = fJason[j][1]["jdBaselines"].bslnA3check === fJason[j][1]["kaBaselines"].bslnB
    // var sameBslnB-alt = fJason[j][1]["jdBaselines"].bslnA3check === fJason[j][1].stabilityComparison

    if(curFocus==="WHAT_Ql"){
        cptDiffA1_WHAT_Ql+= 1*sameBslnA1, cptDiffA2_WHAT_Ql+= 1*sameBslnA2, cptDiffA3_WHAT_Ql+= 1*sameBslnA3
    } else if(curFocus==="WHAT_Qn"){
        cptDiffA1_WHAT_Qn+= 1*sameBslnA1, cptDiffA2_WHAT_Qn+= 1*sameBslnA2, cptDiffA3_WHAT_Qn+= 1*sameBslnA3
    } else if(curFocus==="WHERE"){
        cptDiffA1_WHERE+= 1*sameBslnA1, cptDiffA2_WHERE+= 1*sameBslnA2, cptDiffA3_WHERE+= 1*sameBslnA3
    } else {
        console.error("We don't have a focus that matches our expectations.");
    }

    fJason[j].diff_ka_jd = {
       "baselineA1" : fJason[j][1]["jdBaselines"].bslnA1check - fJason[j][1]["kaBaselines"].bslnA1,
       "baselineA1" : fJason[j][1]["jdBaselines"].bslnA2check - fJason[j][1]["kaBaselines"].bslnA2,
       "baselineA1" : fJason[j][1]["jdBaselines"].bslnA3check - fJason[j][1]["kaBaselines"].bslnA3
    }


}
console.log("loop done")

console.log({cptDiffA1_WHAT_Ql,cptDiffA2_WHAT_Ql,cptDiffA3_WHAT_Ql, cptDiffB_WHAT_Ql,
    cptDiffA1_WHAT_Qn,cptDiffA2_WHAT_Qn,cptDiffA3_WHAT_Qn, cptDiffB_WHAT_Qn,
    cptDiffA1_WHERE, cptDiffA2_WHERE, cptDiffA3_WHERE, cptDiffB_WHERE})

var d = new Date();
var strTime = d.getFullYear()+"_"+(d.getMonth()+1)+"_"+d.getUTCDate()+"_"+d.getHours();
fs.writeFile("Data_2022_05_12/Outputs/dataMap_"+strTime + ".json", JSON.stringify(fJason), function (err, data) { if (err) console.log('error', err); });
