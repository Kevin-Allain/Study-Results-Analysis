// Add_Second_Correction_SC is a program set to open up the data file and modify each error where the differences of stability are not matching the label

console.log("---- filterAccordingToSC.js")
// https://paper.dropbox.com/doc/Notes-about-coding-for-results-analysis--BIcFyz4C8sfOP9sNxjJhTANNAQ-2QtCwBvX4Vng9cWFJjlds
var $ = require("jquery");
const csvparser = require('csv-parser');
const fs = require('fs');
// I'm going to transform the csv into json, modify it accordingly, and then create a new csv.
const csv = require('csvtojson')

// note that maximal absolute value for SC is 0.7
var scValue = 5
var filterType = "percentage"
const readline = require('readline');
const { exit } = require("process");

const inquirer = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

try{
    inquirer.question("Do you want filter to be absolute or based on percentfilter_value? Reply (a/p)", filter_type => {
    inquirer.question("What's the value of the filter?", filter_value => {
        console.log(`Filter is ${filter_type} and value is ${filter_value}`);
        filterType= (filter_type==="a")?"absolute":(filter_type==="p")?"percentage":null;
        scValue=Number(filter_value)
        if(filterType===null){ throw "exit"; }

        // filter_type of the file for which we need to add a better indication... enriched_stimuli_2022_03_23.json
        let fileData=fs.readFileSync('data/update_2022_post_writing/fileData2022_4_28.json','utf8') // take a different file
        fileData = JSON.parse(fileData)
        console.log(typeof fileData)
        console.log(fileData[0])
        console.log(fileData[0].bslnB == false)

        var absSC_Qn = scValue, absSC_Ql = scValue, absSC_Where = scValue;

        if (filterType==="percentage")
        {
            // Get, according to focus, the absolute max of numDiffSC
            var maxSC_Qn = 0, maxSC_Ql = 0, maxSC_Where = 0;
            for (var k in fileData){
                var curFocus = fileData[k]["focus"]
                var toCompareSC = (curFocus=="WHAT_Qn")?maxSC_Qn:(curFocus=="WHAT_Ql")?maxSC_Ql:maxSC_Where;
                if(toCompareSC<=Math.abs(fileData[k]["numDiffSC"])){
                    toCompareSC=Math.abs(fileData[k]["numDiffSC"]);
                    if(curFocus=="WHAT_Qn"){
                        maxSC_Qn=toCompareSC
                    }
                    else if (curFocus=="WHAT_Ql"){
                        maxSC_Ql=toCompareSC
                    }
                    else{
                        maxSC_Where=toCompareSC
                    }
                }
            }

            console.log({maxSC_Qn,maxSC_Ql,maxSC_Where})
            absSC_Qn = maxSC_Qn* scValue/100, absSC_Ql = maxSC_Ql* scValue/100, absSC_Where = maxSC_Where* scValue/100;
        }
        console.log({absSC_Qn,absSC_Ql,absSC_Where})

        var filteredFile = []
        for (var k in fileData){
            var curFocus = fileData[k]["focus"]
            var curFilter = (curFocus=="WHAT_Qn")?absSC_Qn:(curFocus=="WHAT_Ql")?absSC_Ql:absSC_Where;
            if(Math.abs(fileData[k]["numDiffSC"]) >= curFilter )
            {
                filteredFile.push(fileData[k])
            }
        }
        filteredFile = JSON.stringify(filteredFile)
        var d = new Date()
        var strDate = d.getFullYear()+"_"+(d.getMonth()+1)+"_"+d.getDate()

        fs.writeFile("data/update_2022_post_writing/fileData_"+filterType+scValue+"_"+strDate+".json", filteredFile, function (err, data) {
            if (err) console.log('error', err);
        });

        inquirer.close();
    });
    });

    inquirer.on("close", function() {
    console.log("Good bye!");
    process.exit(0);
    });
} catch(e){
    console.log(e);
}
