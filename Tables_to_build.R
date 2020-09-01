
            #                            A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U
            BICYCLE_THEFT            = c(T, F, F, F, F, F, F, F, T, F, F, T, T, F, F, F, F, F, F, F, F)
            BURGLARY                 = c(T, F, T, T, T, F, T, T, T, F, F, T, T, T, F, T, F, F, F, F, F)
            CRIMINAL_DAMAGE          = c(T, F, F, F, T, F, T, F, F, F, F, T, T, F, F, T, F, F, F, F, F)
            OTHER_HOUSEHOLD_THEFT    = c(T, F, F, F, F, F, F, T, T, F, F, T, T, F, F, F, F, F, F, F, F)
            PERSONAL_AND_OTHER_THEFT = c(T, F, F, F, F, F, F, T, T, F, F, T, T, F, F, F, F, F, F, F, F)
            ROBBERY                  = c(T, T, F, F, F, F, F, F, F, T, T, T, T, F, T, T, T, T, F, F, F)
            VEHICLE_RELATED_THEFT    = c(T, F, F, F, F, F, T, T, T, F, F, T, T, F, F, F, F, F, F, T, F)
            VIOLENCE                 = c(T, T, F, F, F, F, F, F, F, T, T, T, T, F, T, T, T, T, F, F, F)
            
            # A = Timing                                   L = Emotional impact
            # B = Where incident occurred                  M = Perceived seriousness
            # C = Point of entry                           N = Contact with offenders
            # D = Method of entry                          O = Number of incidents
            # E = Type of damage                           P = Offender characteristics
            # F = Stolen vehicles returned to owner        Q = Perception of offenders under the influence
            # G = Cost of damage                           R = Number of offenders under the influence
            # H = Items stolen                             S = Was the bike locked
            # I = Cost of items stolen                     T = Age of vehicle stolen
            # J = Injuries sustained                       U = Vehicle security
            # K = Use of weapons

            autotable_functions_corresponding_to_the_columns = c("timing",
                                                                 "where_incident_occurred",
                                                                 "point_of_entry",
                                                                 "method_of_entry",
                                                                 "type_of_damage",
                                                                 "stolen_vehicles_returned_to_owner",
                                                                 "cost_of_damage",
                                                                 "items_stolen",
                                                                 "cost_of_items_stolen",
                                                                 "injuries_sustained",
                                                                 "use_of_weapons",
                                                                 "emotional_impact",
                                                                 "perceived_seriousness",
                                                                 "contact_with_offenders",
                                                                 "number_of_incidents_against_men_and_women",
                                                                 "offender_characteristics", 
                                                                 "proportion_of_offenders_under_the_influence",
                                                                 "number_of_offenders_under_the_influence",
                                                                 "was_the_bike_locked",
                                                                 "age_of_vehicle_stolen",
                                                                 "vehicle_security")


# 1. Row-bind the vectors above into a logical matrix:
tables_to_build = rbind(BICYCLE_THEFT, BURGLARY, CRIMINAL_DAMAGE, OTHER_HOUSEHOLD_THEFT,
                        PERSONAL_AND_OTHER_THEFT, ROBBERY, VEHICLE_RELATED_THEFT, VIOLENCE)

# 2. Set up an empty list of 8 lists, to store the tables as we build them:
tables_built = rep(list(vector(mode = "list", length = ncol(tables_to_build))), nrow(tables_to_build))

# 3. Name the columns of the matrix after the autotable functions that will build the respective tables:
colnames(tables_to_build) = autotable_functions_corresponding_to_the_columns