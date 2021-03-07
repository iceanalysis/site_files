var page = require('webpage').create();
                       page.open('http://ontariohockeyleague.com/gamecentre/23212/boxscore', function () {
                       console.log(page.content); //page source
                       phantom.exit();
                       });
