update main.experiment_user u
set profile_image = (select f.profile_image from (select max(profile_image) profile_image, username from main.experiment_user_shortest group by username) f where f.username = u.username )

update main.experiment_user u
set background_image = (select f.background_image from (select max(background_image) background_image, username from main.experiment_user_shortest group by username) f where f.username = u.username )

update main.experiment_user u
set profile_image_type = (
	select case when f.def = 1 then 'default' when f.cnt = 1 then 'unique' else 'notunique' end from 
	(
		select max(profile_image) profile_image, count(profile_image_uniqueness) as cnt, max(is_default_profile_image) as def, username from main.experiment_user_shortest group by username	
	) 
	f
	where f.username = u.username
	)

update main.experiment_user u
set background_image_type = (
	select case when f.def = 1 then 'default' when f.th > 0 then 'theme' when f.cnt = 1 then 'unique' else 'notunique' end 
	from 
	(
		select max(background_image) background_image, count(background_image_uniqueness) as cnt, max(is_default_background_image) as def, max(is_theme_background_image) as th, username from main.experiment_user_shortest group by username	
	) 
	f
	where f.username = u.username
	)
	
select *
from main.experiment_user