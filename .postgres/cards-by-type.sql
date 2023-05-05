-- data for cards by type page
create or replace view v_cards_by_type as
select b.useremail as grouptype, b.setcode, b.setname, b.releasedate, b.type as cardtype, b.color,
	count(b.uuid) as numcards, sum(b.avgretailprice) as avgretailprice
  from v_user_cards b
    group by b.useremail, b.setcode, b.setname, b.releasedate, b.type, b.color
union all
select 'all' as grouptype, b.setcode, b.setname, b.releasedate, b.type as cardtype, b.color,
	count(b.uuid) as numcards, sum(b.avgretailprice) as avgretailprice
  from v_set_cards b
    group by b.setcode, b.setname, b.releasedate, b.type, b.color;
	

	
	select b.* from v_cards_by_type b
	join v_user_sets c on (b.setcode = c.setcode)
	where b.grouptype in ('stacy@email.com', 'all')
	and c.useremail = 'stacy@email.com'
	order by setcode, grouptype, cardtype;