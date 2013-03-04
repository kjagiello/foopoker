module.exports = function(grunt) {

    // Project configuration.
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        less: {
            development: {
                options: {
                    paths: ["static/css"]
                },
                files: {
                    "static/css/main.css": "static/css/main.less"
                }
            },
            production: {
                options: {
                    paths: ["static/css"],
                    yuicompress: true
                },
                files: {
                    "static/css/main.css": "static/css/main.less"
                }
            }
        }
    });

    grunt.loadNpmTasks('grunt-contrib-less');

    grunt.registerTask('default', ['less:development']);
};