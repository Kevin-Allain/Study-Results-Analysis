// console.log("k: ",k,", key: ",key,", jsonObj[k][key]: ", jsonObj[k][key],', key.indexOf("_First Click")!==-1: ',key.indexOf("_First Click")!==-1);
                if(key==="ResponseId"){ console.log('key==="ResponseId"');storeResponseId=objJson[k][key]}
                if(key==="Progress"){storeProgress=objJson[k][key]}
                if(key==="StartDate"){storeStartDate=objJson[k][key]} if(key==="EndDate"){storeEndDate=objJson[k][key]}
                if(key==="Duration (in seconds)"){storeDuration_in_seconds=objJson[k][key]}if(key==="Finished"){storeFinished=objJson[k][key]}
                if(key==="RecordedDate"){storeRecordedDate=objJson[k][key]}

                //     console.log("k: ",k,", objJson[k]: ",objJson[k])
                var indxBaseAnswer=-1;
                var numID = (key.indexOf('_')!==-1)?Number(key.substr(1,key.indexOf('_')-1)):Number(key.substr(1));

                if(typeof numID === "number" && numID>15) {
                //             console.log("numID: ",numID,", key: ",key,", objJson[k][key]: ",objJson[k][key],", indxBaseAnswer: ",indxBaseAnswer,', key.indexOf("_Page Submit:")!==-1: ',(key.indexOf("_Page Submit:")!==-1),', objJson[k][key]!=="": ',objJson[k][key]!=="");
                    if (key.indexOf("_Page Submit")!==-1 && objJson[k][key]!==""){

                        var keyQual = "QID"+numID;

                        objGenerated[objGenerated.length-1]["ResponseId"]=storeResponseId; objGenerated[objGenerated.length-1]["Progress"]=storeProgress; objGenerated[objGenerated.length-1]["RecordedDate"]=storeRecordedDate; objGenerated[objGenerated.length-1]["StartDate"]=storeStartDate; objGenerated[objGenerated.length-1]["EndDate"]=storeEndDate; objGenerated[objGenerated.length-1]["Finished"]=storeFinished;objGenerated[objGenerated.length-1]["Duration_in_seconds"]=storeDuration_in_seconds; objGenerated[objGenerated.length-1]["filename"]=QIDtoFilename[keyQual]; 
                        var filename = QIDtoFilename[keyQual]
                        var objInfo = extractColumnsFromFilename(filename);
                        console.log("objInfo: ",objInfo);
                        for(var infoK in objInfo){
                            objGenerated[objGenerated.length-1][infoK] = objInfo[infoK];
                        }


                        indxBaseAnswer = numID;
                        if (indxBaseAnswer!== -1){
                            console.log("potential keys | Q: ",indxBaseAnswer+1,indxBaseAnswer+2,indxBaseAnswer+3,indxBaseAnswer+4,indxBaseAnswer+5,indxBaseAnswer+6,indxBaseAnswer+7,indxBaseAnswer+8)
                            console.log("potential vals | Q: ",objJson[k]['Q'+(indxBaseAnswer+1)+'_1'],objJson[k]['Q'+(indxBaseAnswer+2)+'_1'],objJson[k]['Q'+(indxBaseAnswer+3)+'_1'],objJson[k]['Q'+(indxBaseAnswer+4)],objJson[k]['Q'+(indxBaseAnswer+5)],objJson[k]['Q'+(indxBaseAnswer+6)],objJson[k]['Q'+(indxBaseAnswer+7)],objJson[k]['Q'+(indxBaseAnswer+8)]) 

                            // Update 
                            objGenerated[objGenerated.length-1]["answerA1"] = Number(objJson[k]['Q'+(indxBaseAnswer+1)+'_1']) - Number(objGenerated[objGenerated.length-1]["bslnA1"])
                            objGenerated[objGenerated.length-1]["answerA2"] = Number(objJson[k]['Q'+(indxBaseAnswer+2)+'_1']) - Number(objGenerated[objGenerated.length-1]["bslnA2"])
                            objGenerated[objGenerated.length-1]["answerA3"] = Number(objJson[k]['Q'+(indxBaseAnswer+3)+'_1']) - Number(objGenerated[objGenerated.length-1]["bslnA3"])
                            objGenerated[objGenerated.length-1]["answerB"] = objJson[k]['Q'+(indxBaseAnswer+4)]
                            objGenerated[objGenerated.length-1]["trustA1"] = Number(objJson[k]['Q'+(indxBaseAnswer+5)])
                            objGenerated[objGenerated.length-1]["trustA2"] = Number(objJson[k]['Q'+(indxBaseAnswer+6)])
                            objGenerated[objGenerated.length-1]["trustA3"] = Number(objJson[k]['Q'+(indxBaseAnswer+7)])                             
                            objGenerated[objGenerated.length-1]["trustB"] = Number(objJson[k]['Q'+(indxBaseAnswer+8)])

                            // difference for truth? // We have to consider that slider is different... difference is an okay measurement.
                            // TODO UPDATE
                            objGenerated[objGenerated.length-1]["diffA1"] = Number(objJson[k]['Q'+(indxBaseAnswer+1)+'_1'])
                            objGenerated[objGenerated.length-1]["diffA2"] = Number(objJson[k]['Q'+(indxBaseAnswer+2)+'_1'])
                            objGenerated[objGenerated.length-1]["diffA3"] = Number(objJson[k]['Q'+(indxBaseAnswer+3)+'_1'])
                            objGenerated[objGenerated.length-1]["correctB"] = 1*(objGenerated[objGenerated.length-1]["answerB"]===objGenerated[objGenerated.length-1]["bslnB"])
                            
                        }
                    }
                }




function generateModifiedCSV(QIDtoFilename, csvFilePath, addRandomInfoToFillVoid = false) {
    var objGenerated = [];
    var cptForBundlingQuestions = 0;
    var storeResponseId = "";
    var storeStartDate = "", storeEndDate = "", storeProgress = "", storeDuration_in_seconds = "", storeFinished = "", storeRecordedDate = "";

    var baseIndx_frstClck = 0;

    csv()
        .fromFile(csvFilePath)
        .then((jsonObj) => {
            for (var k in jsonObj) {
                console.log("k: ", k,", jsonObj[k]: ",jsonObj[k]);

                // Keep a boolean to indicate if the QID has a page submit...
                var pagesubmitExists = false;

                for (var key in jsonObj[k]) {
                    // console.log("k: ",k,", key: ",key,", jsonObj[k][key]: ", jsonObj[k][key],', key.indexOf("_First Click")!==-1: ',key.indexOf("_First Click")!==-1);

                    if (key === "ResponseId") { storeResponseId = jsonObj[k][key] }
                    if (key === "Progress") { storeProgress = jsonObj[k][key] }
                    if (key === "StartDate") { storeStartDate = jsonObj[k][key] } if (key === "EndDate") { storeEndDate = jsonObj[k][key] }
                    if (key === "Duration (in seconds)") { storeDuration_in_seconds = jsonObj[k][key] } if (key === "Finished") { storeFinished = jsonObj[k][key] }
                    if (key === "RecordedDate") { storeRecordedDate = jsonObj[k][key] }

                    // Can I find already if the question has been answered?!
                    // console.log("jsonObj[k]['PageSubmit']", jsonObj[k]['PageSubmit']) // undefined // it's not 'PageSubmit' but something like 'Q'+numID+'_PageSubmit'

                    var numID = (key.indexOf('_') !== -1) ? Number(key.substr(1, key.indexOf('_') - 1)) : Number(key.substr(1));
                    // console.log("numID",numID,", numID % 5",(numID % 5),", key: ",key);

                    // Can we verify based on QID the PageSubmit?!
                    // console.log("jsonObj[k]['Q'+numID+'_Page Submit']: ", jsonObj[k]['Q'+numID+'_Page Submit'] )
                    pagesubmitExists = (typeof jsonObj[k]['Q' + numID + '_Page Submit'] !== "undefined") && (jsonObj[k]['Q' + numID + '_Page Submit'] !== '');
                    console.log("numID", numID, ", numID%7", (numID % 7), ", key: ", key);
                    if (key.indexOf("_First Click") !== -1) {
                        baseIndx_frstClck = numID;
                    }

                    console.log("pagesubmitExists: ", pagesubmitExists, ", jsonObj[k]['Q" + numID + "_Page Submit']: ", jsonObj[k]['Q' + numID + '_Page Submit'], ", numID: ", numID, ", baseIndx_frstClck: ", baseIndx_frstClck, ", numID-baseIndx_frstClck: ", (numID - baseIndx_frstClck));

                    if (pagesubmitExists || addRandomInfoToFillVoid) {
                        if (key.indexOf("_First Click") !== -1) {  // New line for a question
                            var keyQual = "QID" + numID;
                            console.log("key has first click, numID: ", numID);

                            objGenerated.push({})
                            objGenerated[objGenerated.length - 1]["ResponseId"] = storeResponseId;
                            objGenerated[objGenerated.length - 1]["Progress"] = storeProgress; objGenerated[objGenerated.length - 1]["RecordedDate"] = storeRecordedDate;
                            objGenerated[objGenerated.length - 1]["StartDate"] = storeStartDate; objGenerated[objGenerated.length - 1]["EndDate"] = storeEndDate;
                            objGenerated[objGenerated.length - 1]["Finished"] = storeFinished; objGenerated[objGenerated.length - 1]["Duration_in_seconds"] = storeDuration_in_seconds;


                            objGenerated[objGenerated.length - 1]["filename"] = QIDtoFilename[keyQual];
                            var filename = QIDtoFilename[keyQual]
                            var objInfo = extractColumnsFromFilename(filename);
                            console.log("objInfo: ", objInfo);
                            for (var infoK in objInfo) {
                                objGenerated[objGenerated.length - 1][infoK] = objInfo[infoK];
                            }
                            console.log("k: ", k, ", key: ", key, ", jsonObj[k][key]: ", jsonObj[k][key], ', key.indexOf("_First Click")!==-1: ', key.indexOf("_First Click") !== -1, ', objGenerated[objGenerated.length-1]: ', objGenerated[objGenerated.length - 1]);

                            (jsonObj[k][key] === '' && addRandomInfoToFillVoid) ? objGenerated[objGenerated.length - 1]["FirstClick"] = Math.random() * 20
                                : objGenerated[objGenerated.length - 1]["FirstClick"] = jsonObj[k][key];

                        } else if (key.indexOf("_Last Click") !== -1) { // Same number, following element: last click
                            (jsonObj[k][key] === '' && addRandomInfoToFillVoid) ? objGenerated[objGenerated.length - 1]["LastClick"] = objGenerated[objGenerated.length - 1]["FirstClick"] + Math.random() * 10
                                : objGenerated[objGenerated.length - 1]["LastClick"] = jsonObj[k][key];

                        } else if (key.indexOf("_Page Submit") !== -1) { // Same number, following element: page submit ... We don't care about number of clicks which is the next one.
                            console.log("key with pagesubmit: ", key);

                            (jsonObj[k][key] === '' && addRandomInfoToFillVoid) ? objGenerated[objGenerated.length - 1]["PageSubmit"] = objGenerated[objGenerated.length - 1]["LastClick"] + Math.random() * 15
                                : objGenerated[objGenerated.length - 1]["PageSubmit"] = jsonObj[k][key];

                        } else if (numID > 15 && baseIndx_frstClck !== 0 && (numID - baseIndx_frstClck === 1)) { // answer1a
                            console.log("numID-baseIndx_frstClck===1... correct1a: ", jsonObj[k][key])
                            objGenerated[objGenerated.length - 1]["correct1a"] = jsonObj[k][key] - objInfo["bslnA1"]; // do we want an absolute value?! Let's guess not...
                        } else if (numID > 15 && baseIndx_frstClck !== 0 && (numID - baseIndx_frstClck === 2)) { // answer1b
                            console.log("numID-baseIndx_frstClck===2... correct1b: ", jsonObj[k][key])
                            objGenerated[objGenerated.length - 1]["correct1b"] = jsonObj[k][key] - objInfo["bslnA2"];
                        } else if (numID > 15 && baseIndx_frstClck !== 0 && (numID - baseIndx_frstClck === 3)) { // answer1c
                            console.log("numID-baseIndx_frstClck===3... correct1c: ", jsonObj[k][key])
                            objGenerated[objGenerated.length - 1]["correct1c"] = jsonObj[k][key] - objInfo["bslnA3"];
                        } else if (numID > 15 && baseIndx_frstClck !== 0 && (numID - baseIndx_frstClck === 4)) { // trust1
                            console.log("numID-baseIndx_frstClck===2... trust1: ", jsonObj[k][key])
                            objGenerated[objGenerated.length - 1]["trust1"] = jsonObj[k][key];
                        } else if (numID > 15 && baseIndx_frstClck !== 0 && (numID - baseIndx_frstClck === 5)) { // answer2
                            console.log("numID-baseIndx_frstClck===2... correct2: ", jsonObj[k][key])
                            objGenerated[objGenerated.length - 1]["correct2"] = jsonObj[k][key];
                        } else if (numID > 15 && baseIndx_frstClck !== 0 && (numID - baseIndx_frstClck === 6)) { // trust2
                            console.log("numID-baseIndx_frstClck===2... trust2: ", jsonObj[k][key])
                            objGenerated[objGenerated.length - 1]["trust2"] = jsonObj[k][key];
                        }
                        // } else if ( numID>15 && numID%7==2 ){
                        //     console.log("numID%7==2: ",numID%7==2,", key: ",key)
                        //     // Adapt for matching with the baseline as numbers: 1,0 and -1 according to agree, neither and disagree
                        //     // Neither agree nor disagree // Disagree // Agree
                        //     var rndNumForGen = Math.random();
                        //     // console.log("jsonObj[k][key]==='' && addRandomInfoToFillVoid: ",jsonObj[k][key]==='' && addRandomInfoToFillVoid);
                        //     objGenerated[objGenerated.length-1]["QuestionA"] = (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? ((rndNumForGen<0.33)?-1:(rndNumForGen<0.66)?0:1)
                        //     : ((jsonObj[k][key].indexOf("Neither agree nor disagree")!==-1)?0:(jsonObj[k][key].indexOf("Disagree")!==-1)?-1:(jsonObj[k][key].indexOf("Agree")!==-1)?1:'');

                        //     // Add if answer is correct! 
                        //     objGenerated[objGenerated.length-1]["correctA"] = 1*(objGenerated[objGenerated.length-1]["QuestionA"]===objGenerated[objGenerated.length-1]["bslnA"])

                        // }else if ( numID>15 && numID%7==3 ){
                        //     console.log("numID%7==3: ",numID%7==3,", key: ",key)
                        //     var rndNumForGen = Math.random()
                        //     objGenerated[objGenerated.length-1]["TrustA"] = (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? (Math.floor(Math.random()*6))
                        //     : jsonObj[k][key];

                        // }else if ( numID>15 && numID%7==4 ){
                        //     console.log("numID%7==4: ",numID%7==4,", key: ",key )
                        //     // Adapt for matching with the baseline as numbers: 1,0 and -1 according to agree, neither and disagree
                        //     var rndNumForGen = Math.random()

                        //     objGenerated[objGenerated.length-1]["QuestionB"] = (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? ((rndNumForGen<0.33)?-1:(rndNumForGen<0.66)?0:1)
                        //     : ((jsonObj[k][key].indexOf("Neither agree nor disagree")!==-1)?0:(jsonObj[k][key].indexOf("Disagree")!==-1)?-1:(jsonObj[k][key].indexOf("Agree")!==-1)?1:'');

                        // } else if ( numID>15 && numID%7==0 ){
                        //     console.log("numID%7==0: ",numID%7==0,", key: ",key)
                        //     var rndNumForGen = Math.random()
                        //     objGenerated[objGenerated.length-1]["TrustB"] = (jsonObj[k][key]==='' && addRandomInfoToFillVoid)? (Math.floor(Math.random()*6))
                        //     : jsonObj[k][key];

                        //     // Add if answer is correct!
                        //     objGenerated[objGenerated.length-1]["correctB"] = 1*(objGenerated[objGenerated.length-1]["QuestionB"]===objGenerated[objGenerated.length-1]["bslnB"])

                        // }


                    }
                }
            }

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
            fs.writeFile("data/transformed/survey_precise-study_" + fileNameTime + ".csv", csvOutput, function (err, data) {
                if (err) console.log('error', err);
            });

        })
}
