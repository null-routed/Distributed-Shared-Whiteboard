package it.unipi.dsmt.jakartaee.app.dto;

import org.jetbrains.annotations.NotNull;
import java.io.Serializable;


/**
 * DTO used for sending the user's login information to a remote EJB object.
 */
public class LoginInformationsDTO implements Serializable {

    private String username;
    private String password;

    public LoginInformationsDTO () {}
    
    public LoginInformationsDTO (@NotNull String username, @NotNull String password) {
        this.username = username;
        this.password = password;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}
