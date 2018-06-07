CREATE OR REPLACE FUNCTION pseudo_encrypt(VALUE int) returns int AS $$
DECLARE
l1 int;
l2 int;
r1 int;
r2 int;
i int:=0;
BEGIN
 l1:= (VALUE >> 16) & 65535;
 r1:= VALUE & 65535;
 WHILE i < 3 LOOP
   l2 := r1;
   r2 := l1 # ((((1366 * r1 + 150889) % 714025) / 714025.0) * 32767)::int;
   l1 := l2;
   r1 := r2;
   i := i + 1;
 END LOOP;
 RETURN ((r1 << 16) + l1);
END;
$$ LANGUAGE plpgsql strict immutable;

CREATE OR REPLACE FUNCTION fishyear(TIMESTAMPTZ)
RETURNS TEXT AS $$
    SELECT  CASE WHEN EXTRACT(MONTH FROM $1) < 10
                 THEN (EXTRACT(YEAR FROM $1)::INT)::TEXT
                 ELSE (EXTRACT(YEAR FROM $1)::INT + 1)::TEXT
            END;
$$ LANGUAGE SQL IMMUTABLE;

DROP TABLE IF EXISTS fms2_nt;
CREATE TABLE fms2_nt AS
    SELECT DISTINCT event_key
    FROM kahawai_warehou.landings
    WHERE fishstock_code IN ('GUR2')
    AND fishyear(landing_datetime) > '2007'
    AND fishyear(landing_datetime) < '2018'
  UNION
    SELECT DISTINCT event_key
    FROM kahawai_warehou.effort
    WHERE start_stats_area_code IN ('011','012','013','014','015','016')
      AND fishyear(start_datetime) > '2007'
      AND fishyear(start_datetime) < '2018'
  ;

DROP TABLE IF EXISTS  fms2_ot;
CREATE TABLE fms2_ot AS
    SELECT DISTINCT event_key
    FROM kahawai_historic.landings
    WHERE fishstock_code IN ('GUR2')
    AND fishyear(landing_datetime) < '2008'
  UNION
    SELECT DISTINCT event_key
    FROM kahawai_historic.effort
    WHERE start_stats_area_code IN ('011','012','013','014','015','016')
    AND fishyear(start_datetime)  < '2008'
  ;

CREATE TABLE n_fma2_fi AS
  SELECT  event_key, version_seqno, trip, fishyear, EXTRACT(month FROM start_datetime) AS month, primary_method, target_species, start_stats_area_code, vessel_key, form_type, DATE(start_datetime), "time"(start_datetime), effort_height, effort_width, effort_length, effort_speed, surface_temp, bottom_depth, start_latitude, start_longitude,  fishing_duration::NUMERIC, effort_num, effort_total_num, total_hook_num, checks, vessel_name
  FROM kahawai_warehou.effort JOIN fms2_nt USING(event_key)
  GROUP BY  event_key, version_seqno, trip, fishyear, month, primary_method, target_species, start_stats_area_code, vessel_key, form_type, DATE(start_datetime), "time"(start_datetime), effort_height, effort_width, effort_length, effort_speed, surface_temp, bottom_depth, start_latitude, start_longitude,  fishing_duration::NUMERIC, effort_num, effort_total_num, total_hook_num, checks, vessel_name;

CREATE TABLE o_fma2_fi AS
  SELECT vessel_key, event_key, version_seqno, trip, fishyear(start_datetime) AS fishyear , EXTRACT(month FROM start_datetime) AS month, primary_method, target_species, start_stats_area_code, form_type, DATE(start_datetime), "time"(start_datetime), effort_height, effort_width, effort_length, effort_speed, surface_temp, bottom_depth, start_latitude, start_longitude, fishing_duration::NUMERIC, effort_num, effort_total_num, total_hook_num, checks, vessel_name
  FROM kahawai_historic.effort JOIN fms2_ot USING(event_key)
  GROUP BY vessel_key, event_key, version_seqno, trip, fishyear, start_datetime, month, primary_method, target_species, start_stats_area_code, form_type, DATE(start_datetime), "time"(start_datetime), effort_height, effort_width, effort_length, effort_speed, surface_temp, bottom_depth, start_latitude, start_longitude, fishing_duration::NUMERIC, effort_num, effort_total_num, total_hook_num, checks, vessel_name;

DROP TABLE IF EXISTS fma2_fi;
CREATE TABLE fma2_fi AS
  SELECT event_key, version_seqno, trip, fishyear, month, primary_method, target_species, start_stats_area_code, vessel_key, form_type, date, time, effort_height, effort_width, effort_length, effort_speed, surface_temp, bottom_depth, start_latitude, start_longitude,  fishing_duration::NUMERIC, effort_num, effort_total_num, total_hook_num, checks, vessel_name
  FROM n_fma2_fi
UNION
  SELECT event_key, version_seqno, trip, fishyear, month, primary_method, target_species, start_stats_area_code, vessel_key, form_type, date, time, effort_height, effort_width, effort_length, effort_speed, surface_temp, bottom_depth, start_latitude, start_longitude,  fishing_duration::NUMERIC, effort_num, effort_total_num, total_hook_num, checks, vessel_name
  FROM o_fma2_fi
  ;

DROP TABLE IF EXISTS  fma2_ca;
CREATE TABLE fma2_ca AS
  SELECT event_key, version_seqno, group_key, species_code, catch_weight
  FROM kahawai_warehou.est_catch JOIN fms2_nt USING(event_key)
  WHERE species_code IN ('GUR')
UNION
  SELECT event_key, version_seqno, group_key, species_code, catch_weight
  FROM kahawai_fma2ins.est_catch JOIN fms2_ot USING(event_key)
  WHERE species_code IN ('GUR');

DROP TABLE IF EXISTS  fma2_la;
CREATE TABLE fma2_la AS
  SELECT event_key, version_seqno, group_key, specprod_seqno, landing_datetime, fishyear(landing_datetime) AS fishyear, landing_name, species_code, fishstock_code, state_code, destination_type, unit_type, unit_num, unit_num_latest, unit_num_other, unit_weight, conv_factor, green_weight, green_weight_type, dcf_key, form_type, trip, vessel_key, checks
  FROM  kahawai_warehou.landings
  WHERE fishstock_code IN ('GUR2')
UNION
  SELECT event_key, version_seqno, group_key, specprod_seqno, landing_datetime, fishyear(landing_datetime) AS fishyear, landing_name, species_code, fishstock_code, state_code, destination_type, unit_type, unit_num, unit_num_latest, unit_num_other, unit_weight, conv_factor, green_weight, green_weight_type, dcf_key, form_type, trip, vessel_key, checks
  FROM kahawai_historic.landings
  WHERE fishstock_code IN ('GUR2');

DROP TABLE IF EXISTS  fma2_la_trips;
CREATE TABLE fma2_la_trips AS
  SELECt DISTINCT trip
  FROM fma2_la;

DROP TABLE IF EXISTS  dropped_landings;
CREATE TABLE dropped_landings AS
SELECT vessel_key, fishing_year, fishstock_code, checks, green_weight, landing_name, form_type, fishyear AS fyear
  FROM  kahawai_warehou.deleted_landings JOIN fma2_la_trips USING(trip)
  WHERE fishstock_code='GUR2'
UNION
  SELECT vessel_key, fishing_year, fishstock_code, checks, green_weight, landing_name, form_type, fishyear(landing_datetime) AS fyear
  FROM kahawai_fma2ins.deleted_landings JOIN fma2_la_trips USING(trip)
  WHERE fishstock_code='GUR2';

DROP TABLE IF EXISTS  fma2_la_ug;
CREATE TABLE fma2_la_ug AS
  SELECT event_key, version_seqno, group_key, specprod_seqno, landing_datetime,  fishyear, landing_name, species_code, fishstock_code, state_code, destination_type, unit_type, unit_num, unit_num_latest, unit_num_other, unit_weight, conv_factor, green_weight, green_weight_type, dcf_key, form_type, trip, vessel_id
  FROM  kahawai_warehou.raw_landings
  WHERE fishstock_code IN ('GUR2')
UNION
  SELECT event_key, version_seqno, group_key, specprod_seqno, landing_datetime, fishyear, landing_name, species_code, fishstock_code, state_code, destination_type, unit_type, unit_num, unit_num_latest, unit_num_other, unit_weight, conv_factor, green_weight, green_weight_type, dcf_key, form_type, trip, vessel_id
  FROM kahawai_fma2ins.raw__fma2ins_la
  WHERE fishstock_code IN ('GUR2');

DROP TABLE IF EXISTS  fma2_mq;
CREATE TABLE fma2_mq AS
  SELECT stock_code, year, month, quantity
  FROM kahawai_fma2ins.fma2ins_mq
  WHERE stock_code IN ('GUR2')
  GROUP BY stock_code, year, month, quantity
UNION
   SELECT REPLACE(fishstock, ' ', '') AS stock_code, year, month, quantity
   FROM kahawai_qmr."10975_QMR"
   WHERE REPLACE(fishstock, ' ', '') IN ('GUR2')
   GROUP BY stock_code, year, month, quantity;

-- DROP TABLE IF EXISTS alloc;
-- CREATE TABLE alloc AS
-- with missing_catch as (SELECT event_key FROM kahawai_warehou.effort WHERE fishyear IN ('2015', '2016', '2017') AND start_stats_area_code IN ('011', '012', '013', '014', '015', '016'))
--     SELECT event_key, species_code, estimated, catch, events
--     FROM kahawai_warehou.alloc_events_prop JOIN missing_catch USING(event_key)
--     WHERE species_code IN ('GUR');

DROP TABLE IF EXISTS allocated;
CREATE TABLE allocated AS
  SELECT *
  FROM kahawai_warehou.alloc_events_prop JOIN (SELECT event_key FROM fma2_fi) AS events USING(event_key)
  WHERE species_code IN ('GUR')
UNION
  SELECT *
  FROM kahawai_fma2ins.alloc_events_prop JOIN (SELECT event_key FROM o_fma2_fi) AS events USING(event_key)
  WHERE species_code IN ('GUR');


DROP TABLE IF EXISTS prop_land;
CREATE TABLE prop_land AS
  SELECT *
  FROM fma2_fi LEFT OUTER JOIN(
      SELECT event_key, species_code, estimated, catch, events
      FROM allocated
    ) AS alloc_land USING (event_key)
  WHERE primary_method = 'BT';



CREATE TABLE vessel_char AS
  SELECT *
  FROM kahawai_warehou.vessel_history
  WHERE vessel_name IN (SELECT DISTINCT vessel_name FROM fma2_fi);


  DROP TABLE IF EXISTS historic_method;
  CREATE TABLE historic_method AS
    SELECT  trip,
            fishyear,
            primary_method
    FROM kahawai_historic.effort JOIN fma2_la_trips USING(trip)
    WHERE start_stats_area_code IN ('011', '012', '013', '014', '015', '016')
    GROUP BY  trip,
            fishyear,
            primary_method
  UNION
    SELECT  trip,
            fishyear,
            primary_method
    FROM kahawai_warehou.effort JOIN fma2_la_trips USING(trip)
    WHERE start_stats_area_code IN ('011', '012', '013', '014', '015', '016')
    GROUP BY  trip,
            fishyear,
            primary_method;
