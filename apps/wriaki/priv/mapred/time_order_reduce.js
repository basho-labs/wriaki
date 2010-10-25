// This function is used as the reduce phase implementation for
// ordering/paging archives in the history view (TODO)
function(v) {
    return v.sort(
        function(a,b) { return b[2]-a[2]; }
    );
}
