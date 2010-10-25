// This function is used as the map phase implementation for
// extracting summary data from archives for history production
// Each element in the result should be a structure of
// {
//   version: (string),    // the hash version of the revision
//   timestamp: (integer), // the time of the revision
//   message: (string),    // the commit message
//   editor: (string),     // the username of the editor
// }
function(v) {
    var json = JSON.parse(v.values[0].data);
    var summary = {
        version: json.version,
        timestamp: json.timestamp,
        message: json.message,
    };
    var links = v.values[0].metadata.Links;
    for(var i in links) {
        if (links[i][2] == "editor") {
            summary.editor = links[i][1];
        }
    }
    return [summary];
}
