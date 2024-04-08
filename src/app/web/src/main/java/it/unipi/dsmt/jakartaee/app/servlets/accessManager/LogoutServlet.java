package it.unipi.dsmt.jakartaee.app.servlets.accessManager;

import it.unipi.dsmt.jakartaee.app.utility.ClientRedirector;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;


/**
 * Servlet to handle logout request.
 */
@WebServlet(name = "LogoutServlet", value = "/logout")
public class LogoutServlet extends HttpServlet {

    private void finalizeLogout(HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("@LogoutServlet: called finalizeLogout() method");
        request.getSession().invalidate();      // Invalidate the session
        ClientRedirector.redirectToLogin(request, response);        // Redirect to login page
    }

    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        finalizeLogout(request, response);
    }

    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        finalizeLogout(request, response);
    }

}