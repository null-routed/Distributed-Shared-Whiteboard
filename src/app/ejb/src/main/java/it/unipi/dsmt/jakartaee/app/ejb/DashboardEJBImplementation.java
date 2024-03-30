package it.unipi.dsmt.jakartaee.app.ejb;

import it.unipi.dsmt.jakartaee.app.dto.DashboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.dto.DashboardDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.DashboardEJB;
import jakarta.annotation.Resource;

import javax.sql.DataSource;
import java.util.List;

public class DashboardEJBImplementation implements DashboardEJB {
    // Data source to MySQL database
    @Resource(lookup = "jdbc/SharedWhiteboardsPool")
    private DataSource dataSource;
    @Override
    public boolean createCourse(DashboardCreationDTO dashboard) {
        return false;
    }

    @Override
    public List<DashboardDTO> searchDashboards(String name) {
        return null;
    }

    @Override
    public List<DashboardDTO> getAllDashboards() {
        return null;
    }

    @Override
    public List<DashboardDTO> getSharedDashboards(String id) {
        return null;
    }

    @Override
    public boolean addSharedDashboard(String username, int dashboardId) {
        return false;
    }

    @Override
    public boolean removeSharedDashboard(String username, int dashboardId) {
        return false;
    }

    @Override
    public boolean deleteDashboard(int id) {
        return false;
    }
}
