console.log("---- csv to JSON")

var $ = require("jquery");
const csvparser = require("csv-parser");
const fs = require("fs");
const csv = require("csvtojson");
const { FORMERR } = require("dns");

console.log("loaded bases")

// TODO put a system to ask user which file to change
// imperfect, but why not...
var d = new Date();
var strTime = d.getFullYear()+"_"+(d.getMonth()+1)+"_"+d.getUTCDate()+"_"+d.getHours();
var fileAccordingToTime = "noFilter_survey_measurement_all_headerAdapted_flipWritten_"+strTime;
// var csvFilePath = "Data_2022_05_12/Outputs/noFilter_survey_measurement_all_headerAdapted_flipWritten_2022_5_16_19.csv"
var csvFilePath = "Data_2022_05_12/Outputs/"+fileAccordingToTime+".csv";

csv()
    .fromFile(csvFilePath)
    .then((objJson) => {

        for(var k in objJson){
            for(var key in objJson[k]){
                if(typeof objJson[k][key] === "string"){
                    // console.log("objJson[",k,"][",key,"]: ",objJson[k][key])
                    if (objJson[k][key].toString().toUpperCase() === "TRUE") objJson[k][key] = !!true;
                    if (objJson[k][key].toString().toUpperCase() === "FALSE") objJson[k][key] = !!false;
                }   
                if( !isNaN(Number(objJson[k][key])) ) objJson[k][key] = Number(objJson[k][key])
            }
        }
        fs.writeFile("Data_2022_05_12/Outputs/"+fileAccordingToTime+".json", JSON.stringify(objJson), function (err, data) { if (err) console.log('error', err); });            
    })