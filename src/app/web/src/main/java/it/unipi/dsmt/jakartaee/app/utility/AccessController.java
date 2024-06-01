package it.unipi.dsmt.jakartaee.app.utility;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;


/**
 * Static class providing methods for controlling user permissions to webapp resources
 * and managing the storage and retrieval in the HTTP session
 * of information about currently logged user.
 */
public class AccessController {

    private static final String LOGGED_USER_ATTRIBUTE = "logged_user";

    /**
     * Get a LoggedUserDTO object corresponding to the currently logged user.
     * If the current user is not logged, it will be redirected to the login page.
     * @param request HttpServletRequest object
     * @param response HttpServletResponse object
     * @return stored instance of LoggedUserDTO if the user is logged, null otherwise
     * @throws IOException if redirection fails
     */
    public static @Nullable LoggedUserDTO getLoggedUserWithRedirect (
            @NotNull HttpServletRequest request,
            @NotNull HttpServletResponse response) throws IOException
    {
        LoggedUserDTO loggedUser = getLoggedUser(request);
        if (loggedUser == null)
            ClientRedirector.redirectToLogin(request, response);        // User not logged => redirect to the login page

        return loggedUser;
    }

    /**
     * Get a LoggedUserDTO object corresponding to the currently logged user.
     * @param request HttpServletRequest object
     * @return stored instance of LoggedUserDTO if the user is logged, null otherwise
     */
    public static @Nullable LoggedUserDTO getLoggedUser (@NotNull HttpServletRequest request) {
        return (LoggedUserDTO) request.getSession().getAttribute(LOGGED_USER_ATTRIBUTE);
    }

    /**
     * Set the stored instance of LoggedUserDTO object.
     * @param request HttpServletRequest object
     * @param loggedUser instance of LoggedUserDTO representing currently logged user
     */
    public static void setLoggedUser (@NotNull HttpServletRequest request, @NotNull LoggedUserDTO loggedUser) {
        request.getSession().setAttribute(LOGGED_USER_ATTRIBUTE, loggedUser);
    }
}
