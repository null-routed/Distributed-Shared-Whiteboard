package it.unipi.dsmt.jakartaee.app.dto;

import java.io.Serializable;

public class DashboardDTO implements Serializable {
    private int id;
    private String name;

    public DashboardDTO(int id, String name) {
        this.id = id;
        this.name = name;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "Course = " + name;
    }
}
