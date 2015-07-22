Dropzone.options.customDropzone = {
    paramName: "file", // The name that will be used to transfer the file
    maxFilesize: 5000, // MB
    parallelUploads: 4,
    uploadMultiple: true,
    addRemoveLinks: true,
    dictDefaultMessage: "droppeth thy files",
    accept: function(file, done) {
        done(); // with no args, true, with args, error with msg

        // if (file.name == "justinbieber.jpg") {
        //     done("Naha, you don't.");
        // }
        // else { done(); }
    }
};
