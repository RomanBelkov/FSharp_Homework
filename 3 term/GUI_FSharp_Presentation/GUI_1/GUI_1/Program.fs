open System.Windows.Forms

Application.EnableVisualStyles()

let form = new Form (Text = "Hello World WinForms")
let button = new Button(Text = "click me!", Dock = DockStyle.Fill)

button.Click.Add (fun _ -> MessageBox.Show("Hello world!", "Hey!") |> ignore) //event handler
form.Controls.Add(button)
form.Show()
//Application.Run(form) //starting event loop
 //the following is an explicit event loop
while form.Created do
    printf "DEBUG"
    Application.DoEvents()
