# app_explore
This app runs online and lets you do some exploration across all the writings. This is built from the work of several people in past semesters, most noticeably Clara's [Project Map](https://clarabrobergseniorproject.netlify.app/posts/final_project/) that explores the locations in all of President Woodruff's journals. As you start thinking about what you can create and what you want to learn about President Woodruff, this can start sparking ideas about places, emotions, and themes he discussed throughout his life.
<br/>

## Parts you'll need to implement into your own work
If you're going to make an app, you'll want to make your own repository so that your files are separate from other apps (Shiny will often try to grab other files, so we keep it separate from other Shiny apps).
It all starts with accessing the Woodruff CSV [https://github.com/wilfordwoodruff/Main-Data/raw/main/data/derived/derived_data.csv](https://github.com/wilfordwoodruff/Main-Data/raw/main/data/derived/derived_data.csv). From here, you can do anything you want! As we transcribe more documents, they'll automatically be added as new rows to this link.

## Saving to R Shiny
If you're creating something that just displays what you've found, you don't need to use Shiny. But, if you want to let people explore an interaction or show a new way of seeing Pres. Woodruff's life, Shiny might be able to help you. In R, go to *File->New File->Shiny App*. You can check out Shiny's tutorials [here](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/). Through R, you can run diagrams and dashboards that are hosted on your computer or online. As you get started, "Run App" will run that app through your computer, but once it's ready you can host it online!
<br/>
The nice thing about Shiny is that it's very flexible. This whole app runs out of only 2 files: **clara_for_shiny.csv**, which we join onto the original dataset with coordinates, and **app.R**, which does all the buttons and display.
## Publishing Online 
The Papers host their website apps through BYU-Idaho's servers. 
1. If you're a student, you can host it with your personal BYU-I account. Make a pull request to add what you've created, and we'll make sure it's visible to people!
2. If you aren't a student, or haven't made an account, we can do that step for you. Make a pull request with **app.R** you've created from your own computer (along with other files if you used them), and we'll publish it through our account to the BYU-I servers and make a link to it from our main page!
