-- data for trades page

select setcode, setname, releasedate, cardname, type, color,
avgbuylistprice, numowned
from v_user_cards
where useremail = 'stacy@email.com';

select setname, sum(avgretailprice) as avgretailprice from (
	select setname, uuid, avgretailprice from v_set_cards
	except
	select setname, uuid, avgretailprice from v_user_cards
	where useremail = 'stacy@email.com'
) as missing_cards
group by setname;