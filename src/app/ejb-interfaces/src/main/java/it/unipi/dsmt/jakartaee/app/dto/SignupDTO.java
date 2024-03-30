package it.unipi.dsmt.jakartaee.app.dto;

import org.jetbrains.annotations.NotNull;
import java.io.Serializable;

/**
 * DTO used for sending the user's signup information to a remote EJB object.
 */
public class SignupDTO implements Serializable {
    private String name;
    private String surname;
    private String email;
    private String username;
    private String password;

    public SignupDTO (@NotNull String name, @NotNull String surname, @NotNull String email, @NotNull String username, @NotNull String password) {
        this.name = name;
        this.surname = surname;
        this.email = email;
        this.username = username;
        this.password = password;
    }

    public String getSurname() {
        return surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
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

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

}
