--final version
SELECT name,
     BID,
     city,
     postal_code,
     latitude,
     longitude,
     stars,
     is_open,
     Caters,
     WiFi,
     Reservation,
     Alcohol,
     Noise,
     Credit_Card,
     Price,
     Friday,
	 Parking,
     Greek,
     Spanish,
	 American,
     French,
     Vietnamese,
     Japanese,
     Chinese,
     Mexican,
     Thai,
     Bars,
     Brunch,
     Fast_Food,
     n_photo,
	 ch_in,
	 date_tip,
	 is_trendy,
	 cum_max_friends,
	 cum_u_names,
	 cum_max_u_elite,
	 cum_max_us_fans,
	 cum_n_tips,
	 cum_max_review_count

FROM
		(SELECT BP.name,
			   BP.BID,
			   BP.city,
			   BP.postal_code,
			   BP.latitude,
			   BP.longitude,
			   BP.stars,
			   BP.is_open,
			   BP.Caters,
			   BP.WiFi,
			   BP.Reservation,
			   BP.Alcohol,
			   BP.Noise,
			   BP.Credit_Card,
			   BP.Price,
			   BP.Friday,
		       BP.Parking,
			   BP.Greek,
			   BP.Spanish,
			   BP.French,
			   BP.Vietnamese,
			   BP.Japanese,
			   BP.Chinese,
			   BP.Mexican,
			   BP.Thai,
		       BP.American,
			   BP.Bars,
			   BP.Brunch,
			   BP.Fast_Food,
			   BP.n_photo,
		       BP.is_trendy
		FROM((
		SELECT BU.name,
			   BU.BID,
			   BU.city,
			   BU.postal_code,
			   BU.latitude,
			   BU.longitude,
			   BU.stars,
			   BU.is_open,
			   BU.Caters,
			   BU.WiFi,
			   BU.Reservation,
			   BU.Alcohol,
			   BU.Noise,
			   BU.Credit_Card,
			   BU.Price,
			   BU.Friday,
			   BU.Parking,
			   BU.Greek,
			   BU.Spanish,
			   BU.French,
			   BU.Vietnamese,
			   BU.Japanese,
			   BU.American,
			   BU.Chinese,
			   BU.Mexican,
			   BU.Thai,
			   BU.Bars,
			   BU.Brunch,
			   BU.Fast_Food,
			   BU.is_trendy
			   FROM
				 (SELECT name,
				  business_id AS BID,
				  city,
				  latitude,
				  longitude,
				  postal_code,
				  stars,
				  is_open,
				  (attributes::json)->>'WiFi' AS WiFi,
				  (attributes::json)->>'Alcohol' AS Alcohol,
				  (attributes::json)->>'BusinessAcceptsCreditCards' AS Credit_Card,
				  (attributes::json)->>'RestaurantsReservations' AS Reservation,
				  (attributes::json)->>'NoiseLevel' AS Noise,
				  (attributes::json)->>'BusinessParking' AS Parking,
				  REPLACE(REPLACE(REPLACE(REPLACE((attributes::json) ->> 'Ambience','''','"'),'False','"False"'),'True','"True"'),'None','"None"')::json->>'trendy' AS is_trendy,
				  (attributes::json)->>'RestaurantsPriceRange2' AS Price,
				  (attributes::json)->>'Caters' AS Caters,
				  (hours::json)->>'Friday' AS Friday,
				  (CASE WHEN STRPOS(categories,'Restaurants') = 0 THEN 'no' ELSE 'yes' END)  AS is_restaurant,
				  (CASE WHEN STRPOS(categories,'Greek') = 0 THEN 'no' ELSE 'yes' END) AS Greek,
				  (CASE WHEN STRPOS(categories,'Spanish') = 0 THEN 'no' ELSE 'yes' END) AS Spanish,
				  (CASE WHEN STRPOS(categories,'Italian') = 0 THEN 'no' ELSE 'yes' END) AS Italian,
				  (CASE WHEN STRPOS(categories,'French') = 0 THEN 'no' ELSE 'yes' END) AS French,
				  (CASE WHEN STRPOS(categories,'Vietnamese') = 0 THEN 'no' ELSE 'yes' END) AS Vietnamese,
				  (CASE WHEN STRPOS(categories,'Japanese') = 0 THEN 'no' ELSE 'yes' END) AS Japanese,
				  (CASE WHEN STRPOS(categories,'American') = 0 THEN 'no' ELSE 'yes' END) AS American,
				  (CASE WHEN STRPOS(categories,'Bars') = 0 THEN 'no' ELSE 'yes' END) AS Bars,
				  (CASE WHEN STRPOS(categories,'Chinese') = 0 THEN 'no' ELSE 'yes' END) AS Chinese,
				  (CASE WHEN STRPOS(categories,'Mexican') = 0 THEN 'no' ELSE 'yes' END) AS Mexican,
				  (CASE WHEN STRPOS(categories,'Fast Food') = 0 THEN 'no' ELSE 'yes' END) AS Fast_Food,
				  (CASE WHEN STRPOS(categories,'Brunch') = 0 THEN 'no' ELSE 'yes' END) AS Brunch,
				  (CASE WHEN STRPOS(categories,'Thai') = 0 THEN 'no' ELSE 'yes' END) AS Thai
			   FROM public3.businesstable) AS BU
		WHERE BU.is_restaurant = 'yes' AND BU.city='Mississauga') AS BU1
		LEFT JOIN
		(SELECT business_id AS PID,
			   COUNT(*) AS n_photo
		FROM public3.phototable
		GROUP BY business_id) AS Photo
		ON Photo.PID = BU1.BID) AS BP) AS TABLE1
		,
		(SELECT tip_user1.TUID1 AS ID1,
			  (CASE WHEN CH1.date1 IS NULL THEN 0 ELSE 1 END) AS ch_in,
			   tip_user1.TUdate1 AS date_tip,
			   tip_user1.cum_n_tips AS cum_n_tips,
			   tip_user1.cum_max_friends AS cum_max_friends,
			   tip_user1.cum_u_names AS cum_u_names,
			   tip_user1.cum_max_u_elite AS cum_max_u_elite,
			   tip_user1.cum_max_us_fans AS cum_max_us_fans,
		       tip_user1.cum_max_review_count AS cum_max_review_count
		FROM (	
					SELECT CH.business_id AS CID,
						   CH_date::DATE AS date1
					FROM (
						SELECT public3.checkintable.business_id AS business_id,
							   unnest(string_to_array(DATE, ',')) AS CH_date
						FROM public3.checkintable, public3.businesstable
						WHERE public3.checkintable.business_id = public3.businesstable.business_id AND public3.businesstable.city='Mississauga'
						) AS CH
					GROUP BY CID,date1 ) AS CH1
		RIGHT JOIN
				   (SELECT tip_user.TID1 AS TUID1,
						   tip_user.Tdate1 AS TUDate1,
						   tip_user.n_tips AS n_tips,
						   tip_user.cum_n_tips AS cum_n_tips,
						   (
										SELECT max(max_us_friends) AS cum_max_friends
										FROM (
											SELECT business_id,DATE,max(users.n_friends) AS max_us_friends
											FROM public3.tipstable
											LEFT JOIN 
										   (SELECT user_id AS user_id,array_length(string_to_array(users.friends, ','), 1) AS n_friends
											FROM public3.userstable AS users) AS users 
											ON public3.tipstable.user_id = users.user_id
											GROUP BY business_id,DATE) AS t53
										WHERE t53.business_id = tip_user.TID1 AND t53.DATE::DATE <= tip_user.Tdate1)
									,(
										SELECT STRING_AGG(DISTINCT u_names, ',') AS cum_u_names
										FROM (
											SELECT business_id,DATE,STRING_AGG(DISTINCT users.u_name, ',') AS u_names
											FROM public3.tipstable
											LEFT JOIN 
										   (SELECT user_id AS user_id,name AS u_name
											FROM public3.userstable AS users) AS users 
											ON public3.tipstable.user_id = users.user_id
											GROUP BY business_id,DATE) AS t53
										WHERE t53.business_id = tip_user.TID1 AND t53.DATE::DATE <= tip_user.Tdate1)
									,(
										SELECT max(max_u_elite) AS cum_max_u_elite
										FROM (
											SELECT business_id,DATE,max(users.n_elite) AS max_u_elite
											FROM public3.tipstable
											LEFT JOIN 
										   (SELECT user_id AS user_id,array_length(string_to_array(users.elite, ','), 1) AS n_elite
											FROM public3.userstable AS users) AS users 
											ON public3.tipstable.user_id = users.user_id
											GROUP BY business_id,DATE) AS t53
										WHERE t53.business_id = tip_user.TID1 AND t53.DATE::DATE <= tip_user.Tdate1)
									,(SELECT max(max_us_fans) AS cum_max_us_fans
										FROM (
											SELECT business_id,DATE,max(users.u_fans) AS max_us_fans
											FROM public3.tipstable
											LEFT JOIN 
										   (SELECT user_id AS user_id,fans AS u_fans
											FROM public3.userstable AS users) AS users 
											ON public3.tipstable.user_id = users.user_id
											GROUP BY business_id,DATE) AS t53
										WHERE t53.business_id = tip_user.TID1 AND t53.DATE::DATE <= tip_user.Tdate1)
									,(SELECT max(max_review_count) AS cum_max_review_count
										FROM (
											SELECT business_id,DATE,max(users.review_count) AS max_review_count
											FROM public3.tipstable
											LEFT JOIN 
										   (SELECT user_id AS user_id,review_count AS review_count
											FROM public3.userstable AS users) AS users 
											ON public3.tipstable.user_id = users.user_id
											GROUP BY business_id,DATE) AS t53
										WHERE t53.business_id = tip_user.TID1 AND t53.DATE::DATE <= tip_user.Tdate1)
					FROM (
								SELECT TID1,
									   Tdate1,
									   n_tips,
									   (SELECT COUNT(t51.TEXT)
										FROM public3.tipstable AS t51
										WHERE t51.business_id = TBU1.TID1
										AND t51.DATE::DATE <= TBU1.Tdate1) AS cum_n_tips
								FROM (
									  SELECT TID1, Tdate1, n_tips  
									  FROM (SELECT tip.business_id AS TID1, 
											date_trunc('day', generate_series('2017-07-03'::date, '2017-07-09'::date, '1 day'::interval))::date AS Tdate1
											FROM public3.tipstable AS tip, public3.businesstable AS BU
											WHERE tip.business_id=BU.business_id AND BU.city='Mississauga'
											GROUP BY tip.business_id) AS TBU1
											LEFT JOIN 
										   (SELECT tip.business_id AS TID,
											DATE::DATE AS Tdate,
											COUNT(tip.TEXT) AS n_tips
											FROM public3.tipstable AS tip, public3.businesstable AS BU
											WHERE tip.business_id = BU.business_id AND BU.city='Mississauga'
											GROUP BY tip.business_id, Tdate) AS TBU
											ON TID = TID1 AND Tdate = Tdate1
									) AS TBU1
						) AS tip_user ) AS tip_user1
		ON CH1.date1 = tip_user1.TUdate1 AND tip_user1.TUID1 = CH1.CID
		WHERE cum_n_tips <> 0) AS TABLE2
WHERE TABLE1.BID = TABLE2.ID1

