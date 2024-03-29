package it.unipi.dsmt.jakartaee.app.interfaces;

import it.unipi.dsmt.jakartaee.app.dto.DashboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.dto.DashboardDTO;
import jakarta.ejb.Remote;
import jakarta.validation.constraints.NotNull;

import java.util.List;

@Remote
public interface DashboardEJB {

    boolean createCourse(@NotNull DashboardCreationDTO dashboard);
    List<DashboardDTO> searchDashboards(String name);
    List<DashboardDTO> getAllDashboards();
    List<DashboardDTO> getSharedDashboards(String id);
    boolean addSharedDashboard(@NotNull String username, int dashboardId);
    boolean removeSharedDashboard(@NotNull String username, int dashboardId);
    boolean deleteDashboard(int id);

}
