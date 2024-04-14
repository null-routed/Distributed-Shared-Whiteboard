package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.UserTransaction;

import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Optional;

public abstract class BaseWhiteboardServlet extends HttpServlet {

    @EJB
    protected WhiteboardEJB whiteboardEJB;

    @Resource
    protected UserTransaction userTransaction;

    protected JsonObject createJsonResponse(boolean success, String message) {
        return Json.createObjectBuilder()
                .add("success", success)
                .add("message", message)
                .build();
    }

    protected void sendResponse(HttpServletResponse response, JsonObject jsonResponse) throws IOException {
        response.setStatus(HttpServletResponse.SC_OK);
        try (PrintWriter out = response.getWriter()) {
            out.print(jsonResponse);
            out.flush();
        }
    }

    protected String getParameter(HttpServletRequest request, String paramName) {
        return Optional.ofNullable(request.getParameter(paramName)).orElse("");
    }

}
