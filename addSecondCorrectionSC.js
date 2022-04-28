// Add_Second_Correction_SC is a program set to open up the data file and modify each error where the differences of stability are not matching the label

console.log("---- addSecondCorrectionSC.js")
// https://paper.dropbox.com/doc/Notes-about-coding-for-results-analysis--BIcFyz4C8sfOP9sNxjJhTANNAQ-2QtCwBvX4Vng9cWFJjlds
var $ = require("jquery");
const csvparser = require('csv-parser');
const fs = require('fs');
// I'm going to transform the csv into json, modify it accordingly, and then create a new csv.
const csv = require('csvtojson')

// Name of the file for which we need to add a better indication... enriched_stimuli_2022_03_23.json
let fileData=fs.readFileSync('enriched_stimuli_2022_03_23.json','utf8')
fileData = JSON.parse(fileData)
console.log(typeof fileData)
console.log(fileData[0])
console.log(fileData[0].bslnB == false)

// Note that numDiffSC < 0 indicates that bslnB should be true, and vice-versa.
for (var k in fileData){
    // console.log("k: ",k,", fileData[k]: ",fileData[k])
    fileData[k].stablityComparison = fileData[k].numDiffSC<0
    fileData[k].issueStabilityComparison = fileData[k].stablityComparison != fileData[k].bslnB
}

console.log(fileData[0])


fileData = JSON.stringify(fileData)
var d = new Date()
var strDate = d.getFullYear()+"_"+(d.getMonth()+1)+"_"+d.getDate()

fs.writeFile("data/update_2022_post_writing/fileData"+strDate+".json", fileData, function (err, data) {
    if (err) console.log('error', err);
});
