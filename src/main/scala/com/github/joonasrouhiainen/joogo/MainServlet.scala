package com.github.joonasrouhiainen.joogo

import org.scalatra._
import scalate.ScalateSupport

class MainServlet extends JoogoStack {

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to Scalate</a>.
      </body>
    </html>
  }
  
}
